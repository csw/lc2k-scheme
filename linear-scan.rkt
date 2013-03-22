#lang racket

(require "ir.rkt")
(require "util.rkt")

(provide linear-scan-build-intervals linear-scan-alloc
         analyze-frame
         interval-start interval-end)

;;;; Linear scan register allocator

;; This implements a simple linear scan register allocator as
;; described in Poletto and Sarkar, 1999, and with some of the
;; adjustments discussed in Sagonas and Stenman, 2003.

(define (interval-var i)
  (first i))

(define (interval-start i)
  (second i))

(define (interval-end i)
  (third i))

(define (interval-stmt i)
  (fourth i))

(define (interval-live i)
  (fifth i))

;; linear-scan-build-intervals:
;;
;; Returns a list of intervals, sorted by interval start

(define (linear-scan-build-intervals code)
  (let* ([starts (make-hash)]
         [ends (make-hash)]
         [stmts (list->vector code)])
    (for ([stmt code]
          [i (in-naturals)])
      (let ([dest (ir-dest stmt)])
        (when (and dest (not (hash-has-key? starts dest)))
          (hash-set! starts dest i))
        (for ([src (ir-sources stmt)])
          (hash-set! ends src i))))
    ;; any we haven't set an end for haven't been used at all
    (for ([no-end-for (remove* (dict-keys ends) (dict-keys starts))])
      (hash-set! ends no-end-for (hash-ref starts no-end-for)))
    (cond
     [(= (hash-count starts) (hash-count ends)) #t]
     ;; shouldn't happen
     [(> (hash-count starts) (hash-count ends))
      (error 'linear-scan-build-intervals
             "no end found for: ~v"
             (remove* (dict-keys ends) (dict-keys starts)))]
     [(< (hash-count starts) (hash-count ends))
      (error 'linear-scan-build-intervals
             "no start found for: ~v"
             (remove* (dict-keys starts) (dict-keys ends)))])
    (sort (for/list ([thing (hash-keys starts)])
            (let ([start (hash-ref starts thing)])
              (list thing
                    start
                    (hash-ref ends thing)
                    (vector-ref stmts start))))
          < #:key interval-start)))

;; Returns a list: (assignments intervals info)
;;
;;   assignments: alist dict
;;     keys:   (temp N) | return-addr
;;     values: (register N) | (spill N) | (frame N)
;;
;;   intervals: list of interval record lists, each as follows:
;;     (var interval-begin interval-end asm)
;;     var: a valid key for assignments
;;     interval-begin, interval-end: positions in sasm
;;     asm: record from sasm
;;
;;   info: alist dict

(define (linear-scan-alloc code intervals)
  (let* ([active empty]
         [free '(1 2 3 6)]
         [r (length free)]
         [next-spill-loc 4]
         [locations empty]
         [assignments empty]
         [fixed-loc empty]
         [live-at empty])
    (define (add-active i)
      (set! active
            (sort (cons i active)
                  < #:key interval-end)))
    (define (assign-location var)
      (let* ([fixed (dict-ref fixed-loc var #f)]
             [loc (or fixed
                      (list 'spill next-spill-loc))])
        (unless fixed
          (set! next-spill-loc (add1 next-spill-loc)))
        loc))
    (define (expire-old-intervals i)
      (let-values ([(expired valid)
                    (partition (lambda (j) (< (interval-end j)
                                              (interval-start i)))
                               active)])
        (values valid
                (append (map cadr
                             (filter (curry tagged-list? 'register)
                                     (map (lambda (j)
                                            (dict-ref assignments (interval-var j)))
                                          expired)))
                        free))))
    (define (spill-at-interval i)
      (let ([spill (last active)])
        (if (> (interval-end spill) (interval-end i))
            ;; spill an existing variable
            (let* ([var (interval-var i)]
                   [to-spill (interval-var spill)]
                   [reg (dict-ref assignments to-spill)]
                   [loc (assign-location to-spill)])
              (set! assignments (dict-set* assignments
                                           var reg
                                           to-spill loc))
              (set! active (sort (cons i (remove spill active))
                                 < #:key interval-end)))
            ;; spill this one immediately
            (begin
              (set! assignments
                    (dict-set assignments
                              (interval-var i)
                              (assign-location (interval-var i))))))))
    (for ([i intervals])
      (let ([stmt (interval-stmt i)])
        (match stmt
          ;; explicit register binding / precoloring
          [(list 'bind ref register loc)
           (set! live-at (cons (map (curry dict-ref assignments)
                                    (dict-keys active))
                               live-at))
           (set! assignments (dict-set assignments
                                       ref
                                       (list 'register register)))
           (set! free (remove register free))
           (when loc
             (set! fixed-loc (dict-set fixed-loc ref loc)))
           (add-active i)]
          ;; regular statement
          [else
           (set!-values (active free) (expire-old-intervals i))
           (set! live-at (cons (dict-keys active)
                               live-at))
           (if (= (length active) r)
               ;; spill something
               (spill-at-interval i)
               ;; assign a register now
               (begin
                 (set! assignments (dict-set assignments
                                             (interval-var i)
                                             (list 'register (car free))))
                 (set! free (cdr free))
                 (add-active i)))])))
    (list assignments
          (map (lambda (int live)
                 (append int (list live)))
               intervals
               (reverse live-at))
          (dict-set* empty
                     'fixed-loc fixed-loc
                     'spill-slots next-spill-loc))))

;; analyze-frame: wraps the register allocator, performs extra
;; post-processing and analysis.
;;
;; (analyze-frame code sasm)
;;   code: function definition, as:
;;       (code (arg1 arg2 ...) body-expr)
;;   sasm: low-level 'symbolic assembly' IR from gen-ir
;;
;;   frame-info: alist dict

(define reg-save-locations
  '(((register 1) . (spill 0))
    ((register 2) . (spill 1))
    ((register 3) . (spill 2))
    ((register 6) . (spill 3))))

(define (analyze-frame code sasm alloc-info)
  (let*-values ([(assignments) (car alloc-info)]
                [(intervals)   (cadr alloc-info)]
                [(frame-info)   (caddr alloc-info)]
                [(is-leaf) (not (findf (lambda (stmt)
                                         (or (tagged-list? 'labelcall stmt)
                                             ;; XXX: review
                                             (tagged-list? 'tail-call stmt)
                                             (tagged-list? 'proc-call stmt)))
                                       sasm))]
                [(regs-used) (apply set (filter (curry tagged-list? 'register)
                                                (dict-values assignments)))]
                [(fixed-loc) (dict-ref frame-info 'fixed-loc)]
                [(stack-assignments)
                 (for/list ([(var loc) (in-dict assignments)])
                   (let ([fixed (dict-ref fixed-loc var #f)])
                     (if fixed
                         (cons var fixed)
                         (match loc
                           [(list (or 'spill 'frame) n)
                            (cons var loc)]
                           [(list 'register n)
                            (cons var (dict-ref reg-save-locations loc))]))))]
                [(frame-size) (+ 1 ; for return address
                                 ;; note that this always includes 4
                                 ;; for registers 1, 2, 3, 6
                                 (dict-ref frame-info 'spill-slots))]
                [(has-stack-vars)
                 (findf (compose not (curry tagged-list? 'register))
                        (dict-values assignments))]
                ;; conservative; might want to check for actual
                ;; save/restore generation and work this out later
                ;; TODO: use has-spilled, sort this out
                [(skip-frame-setup) (and is-leaf
                                         ;; (= (set-count regs-used)
                                         ;;    (dict-count assignments))
                                         (not has-stack-vars))])
    (dict-set* frame-info
               'is-leaf is-leaf
               'stack-assignments stack-assignments
               'frame-size frame-size
               'skip-frame-setup skip-frame-setup)))

