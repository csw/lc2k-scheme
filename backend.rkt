#lang racket

(require racket/trace)

(require "types.rkt")
(require "environ.rkt")
(require "linear-scan.rkt")
(require "ir.rkt")
(require "lc2k.rkt")
(require "util.rkt")

;;;; backend: Generates LC2K assembly from low-level IR.

(provide gen-asm)

;; One could argue that this is totally out of control. One would be
;; correct.

;; Stack frame layout:
;;
;; The first three arguments are passed in r1-r3. Any others should be
;; passed on the stack.
;;
;; Offsets given are from the caller's stack pointer, which is
;; callee-save. A leaf function could use this as a frame pointer; a
;; non-leaf function should decrement it to be its own stack pointer.

;; TODO: rewrite this to match reality

(define no-label "      ")

;; gen-asm: Generates actual LC2K assembly from the low-level IR.
;;
;; - Calls linear-scan-alloc to perform register allocation.
;; - Inserts loads and stores as appropriate for stack-resident vars.
;; - Generates function prologue, call and return sequences from
;;   prologue, labelcall, and return directives.
;;
(define (gen-asm code sasm alloc-info frame-info)
  (let*-values
      ([(assignments) (car alloc-info)]
       [(intervals)   (cadr alloc-info)]
       [(asm) empty]
       [(before) empty]
       [(after) empty]
       [(stack-assignments) (dict-ref frame-info 'stack-assignments)]
       [(lazy-load) empty]
       [(pending-label) no-label])
    (define (fmt-asm dir)
      (begin0 (string-join (map (lambda (v)
                                  (~a v #:min-width 7))
                                (cons pending-label dir)))
        (set! pending-label no-label)))
    (define (emit-line l)
      (set! asm (cons l asm)))
    (define (emit-before line)
      (set! before (cons line before)))
    (define (emit-after line)
      (set! after (cons line after)))
    (define/match (loc-stack-offset loc)
      [((list 'frame n)) (+ n (dict-ref frame-info 'frame-size))]
      [((list 'spill n)) (- (dict-ref frame-info 'spill-slots) n)])
    (define (load-spilled reg loc (v #f) (reason "spilled"))
      (fmt-asm (list 'lw sp-reg reg (loc-stack-offset loc)
                     (format "; load ~a value ~a" reason v))))
    (define (store-spilled reg loc (v #f) (reason "spilled"))
      (fmt-asm (list 'sw sp-reg reg (loc-stack-offset loc)
                     (format "; store ~a value ~a" reason v))))
    (define (subst-dest d pos)
      (match d
        [(or (list 'temp _)
             'return-addr)
         (match (dict-ref assignments d)
           ;; no spill
           [(list 'register r)
            r]
           ;; spill this from scratch reg after call
           [(and loc (list (or 'spill 'frame) _))
            (emit-after (store-spilled spill-dest-reg loc d))
            spill-dest-reg])]
        [(list 'register n)
         n]
        [(? integer?) d]))
    (define (subst-src v pos target-reg)
      (match v
        [(or (list 'temp _)
             (list 'local _)
             'return-addr)
         (match (dict-ref assignments v)
           ;; register
           [(list 'register r)
            r]
           ;; spill this from scratch reg after call
           [(and loc
                 (list (or 'spill 'frame) _))
            (emit-before (load-spilled target-reg loc v))
            target-reg])]
        [(list 'register n)
         n]
        [(? integer?) v]))
    ;;
    ;; (reg-vars-live-past pos)
    ;;
    ;; Returns subset of assignments, with only variables in registers
    ;; holding live values before pos which remain live after pos.
    ;;
    (define (reg-vars-live-past pos)
      (let ([reg-vars (filter (compose (curry tagged-list? 'register)
                                       cdr)
                              assignments)])
        (filter (lambda (asg)
                  (let ([interval (assoc (car asg) intervals)])
                    (and interval
                         (< (interval-start interval) pos)
                         (> (interval-end interval) pos))))
                reg-vars)))
    (define (save-regs-for-call pos except)
      (for ([(var reg) (in-dict (dict-remove (reg-vars-live-past pos)
                                             except))])
        (emit-line (store-spilled (register-num reg)
                                  (dict-ref stack-assignments var)
                                  var
                                  "saved"))))
    (define (restore-regs-for-call pos except)
      (for ([(var reg) (in-dict (dict-remove (reg-vars-live-past pos)
                                             except))])
        (emit-line (load-spilled (register-num reg)
                                  (dict-ref stack-assignments var)
                                  var
                                  "restored"))))
    (define (marshal-args args pos)
      ;; Marshals arguments into registers for a function call.
      ;;
      ;; Tricky because we are likely using the arg registers (1-3)
      ;; for variables. If r1 has to move to r3 and r3 has to move to
      ;; r1, the obvious way won't work.
      ;;
      ;; For each one, we see if we have an arg in its destination
      ;; register already. If so, we save that one temporarily.
      ;;
      (define (marshal ctx dest saved)
        (unless (empty? ctx)
          (let* ([arg (car ctx)] ;; var
                 [loc (dict-ref assignments arg)]
                 [remain (cdr ctx)]
                 [conflict (findf (lambda (a)
                                    (equal? (dict-ref assignments a)
                                            (list 'register dest)))
                                  remain)]
                 [was-saved (member arg saved)] ;; were we saved out?
                 [use-loc (if was-saved
                              (dict-ref stack-assignments arg)
                              loc)])
            ;; if something is in the destination register, spill it
            (when conflict
              (emit-line (store-spilled dest
                                        (dict-ref stack-assignments conflict)
                                        #f
                                        "arg")))
            (match use-loc
              [(list 'register n)
               (unless (= n dest)
                 (emit! 'add 0 n dest (format "; marshal arg ~a" dest)))]
              [(? list?)
               (emit-line (load-spilled dest use-loc arg "arg"))])
            (marshal remain (add1 dest) (if conflict
                                            (cons conflict saved)
                                            saved)))))
      (marshal args 1 empty))

    (define (emit-unwrapped! . dir)
      (emit-line (fmt-asm dir)))
    (define (emit! . dir)
      (for-each emit-line (reverse before))
      (apply emit-unwrapped! dir)
      (for-each emit-line (reverse after))
      (set! before empty)
      (set! after empty))

    (for ([dir sasm]
          [pos (in-naturals)])
      (match dir
        [(list 'label (? string? label) trace ...)
         (unless (eq? pending-label no-label)
           (error 'gen-asm "label ~a already pending, setting ~a"
                  pending-label label))
         (set! pending-label label)
         #f]
        ;; bind
        [(list 'bind var start-reg fixed-loc ...)
         (let ([assigned (dict-ref assignments var)])
           (unless (equal? assigned (list 'register start-reg))
             (emit-line (store-spilled start-reg assigned "relocated"))))]
        ;; add, nand
        [(list (and (or 'add 'nand) op) s1 s2 dest)
         (emit! op
                (subst-src s1 pos spill-s1-reg)
                (subst-src s2 pos spill-s2-reg)
                (subst-dest dest pos))]
        ;; lw
        [(list 'lw s1 dest offset comment ...)
         (emit! 'lw
                (subst-src s1 pos spill-s1-reg)
                (subst-dest dest pos)
                offset
                (if comment
                    (car comment)
                    ""))]
        ;; sw
        [(list 'sw s1 s2 offset comment ...)
         (emit! 'sw
                (subst-src s1 pos spill-s1-reg)
                (subst-src s2 pos spill-s2-reg)
                offset)]
        ;; beq
        [(list 'beq s1 s2 comment ...)
         (emit! 'beq
                (subst-src s1 pos spill-s1-reg)
                (subst-src s2 pos spill-s2-reg)
                (if comment
                    (car comment)
                    ""))]
        ;; labelcall
        [(list 'labelcall dest-var (? proc? call-proc) args ...)
         (let ([target-reg spill-s2-reg])
           (save-regs-for-call pos dest-var)
           ;; marshal first 3 args into registers
           (marshal-args args pos)
           (emit! 'lw 0 target-reg (proc-addr-label call-proc)
                  (format "; load address of ~a" (proc-name call-proc)))
           (emit! 'jalr target-reg ret-reg
                  (format "; call ~a" (proc-name call-proc)))
           (match (dict-ref assignments dest-var)
             [(list 'register 1) #t]
             [(list 'register n) (emit! 'add 0 1 n "; store result")]
             [(? list? stack-loc) (emit-line (store-spilled 1 stack-loc))])
           ;; otherwise we wanted the value in r1, and it's there now
           ;; restore live vars
           (restore-regs-for-call pos dest-var))]
        ;; proc-call
        [(list 'proc-call dest-var proc-var args ...)
         (let* ([target-reg spill-s2-reg]
                [proc-reg (subst-src proc-var pos target-reg)])
           (emit! 'lw   0 spill-s1-reg (const-ref pointer-mask)
                  "; load pointer mask")
           (emit! 'nand spill-s1-reg proc-reg target-reg)
           (emit! 'nand target-reg target-reg target-reg)
           (save-regs-for-call pos dest-var)
           ;; marshal first 3 args into registers
           (marshal-args args pos)
           (emit! 'jalr target-reg ret-reg (format "; call ~a" proc-var))
           (match (dict-ref assignments dest-var)
             [(list 'register 1) #t]
             [(list 'register n) (emit! 'add 0 1 n "; store result")]
             [(? list? stack-loc) (emit-line (store-spilled 1 stack-loc))])
           ;; otherwise we wanted the value in r1, and it's there now
           ;; restore live vars
           (restore-regs-for-call pos dest-var))]
        ;; tail-call
        [(list 'tail-call target ret-addr-ref args ...)
         (match target
           ;; self tail call
           [(? string?)
            ;; marshal arguments into registers
            ;; XXX: need to revisit for stack args
            (marshal-args args pos)
            ;; call for side effect, to ensure return addr is in its register
            (subst-src ret-addr-ref pos ret-reg)
            (emit! 'beq 0 0 target
                   "; self-tail-call")]
           ;; tail call by label
           [(struct proc _)
            ;; marshal arguments into registers
            (marshal-args args pos)
            ;; call for side effect, to ensure return addr is in its register
            (subst-src ret-addr-ref pos ret-reg)
            (let ([target-addr-label (proc-addr-label target)]
                  [frame-size (dict-ref frame-info 'frame-size)])
              (unless (dict-ref frame-info 'skip-frame-setup)
                (emit! 'lw 0 spill-s1-reg (const-ref frame-size))
                (emit! 'add sp-reg spill-s1-reg sp-reg
                       (format "; SP += ~a" frame-size)))
              (emit! 'lw 0 spill-s1-reg target-addr-label
                     "; load target address")
              (emit! 'jalr spill-s1-reg spill-s2-reg
                     (format "; tail-call ~a" (proc-name target))))]
           ;; tail call by pointer
           [(list 'local proc-var)
            (let* ([target-reg spill-s2-reg]
                   [proc-reg (subst-src target pos target-reg)]
                   [frame-size (dict-ref frame-info 'frame-size)])
              (emit! 'lw   0 spill-s1-reg (const-ref pointer-mask)
                     "; load pointer mask")
              (emit! 'nand spill-s1-reg proc-reg target-reg)
              (emit! 'nand target-reg target-reg target-reg)
              ;; marshal arguments into registers
              (marshal-args args pos)
              ;; call for side effect, to ensure return addr is in its register
              (subst-src ret-addr-ref pos ret-reg)
              (unless (dict-ref frame-info 'skip-frame-setup)
                (emit! 'lw 0 spill-s1-reg (const-ref frame-size))
                (emit! 'add sp-reg spill-s1-reg sp-reg
                       (format "; SP += ~a" frame-size)))
              (emit! 'jalr target-reg spill-s1-reg
                     (format "; tail-call ~a" proc-var)))])]
        ;; return
        [(list 'return rv-ref addr-ref)
         (let ([rv-cur-reg (subst-src rv-ref pos rv-reg)]
               [addr-reg (subst-src addr-ref pos spill-s2-reg)]
               [frame-size (dict-ref frame-info 'frame-size)])
           (unless (= rv-cur-reg rv-reg)
             (emit! 'add 0 rv-cur-reg rv-reg "; place return value"))
           (unless (dict-ref frame-info 'skip-frame-setup)
             (emit! 'lw 0 spill-s1-reg (const-ref frame-size))
             (emit! 'add sp-reg spill-s1-reg sp-reg
                    (format "; SP += ~a" frame-size)))
           (emit! 'jalr addr-reg spill-s1-reg "; return"))]
        ;; prologue
        [(list 'prologue)
         (unless (dict-ref frame-info 'skip-frame-setup)
           (emit! 'lw 0 spill-s1-reg (const-ref (- (dict-ref frame-info
                                                             'frame-size))))
           (emit! 'add sp-reg spill-s1-reg sp-reg
                  (format "; SP -= ~a" (dict-ref frame-info
                                                 'frame-size))))]
        ;; noop
        [(list 'noop comment ...)
         (apply emit! dir)]))
    (reverse asm)))

