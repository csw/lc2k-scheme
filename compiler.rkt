#lang racket

(require racket/format)

(provide compile-program compile-to *lc2k-rv* decode-immediate)

(define *runtime-text*
  '("        lw   0 6 entry"
    "        lw   0 7 stack"
    "        jalr 6 5"
    "        sw   0 1 SCMrv"
    "SCMh    halt"))

(define *runtime-data*
  (list (format "entry   .fill ~s" (length *runtime-text*))
        "stack   .fill 65535"
        "SCMrv   .fill 559038737"))

(define sp-reg 7)
(define call-reg 6)
(define ret-reg 5)
(define rv-reg 1)

(define *lc2k-rv* "SCMrv")

(define fixnum-mask  #b01000000000000000000000000000000)
(define fixnum-shift 1)
(define tag-mask     #xEF000000)
(define no-tag-mask  (bitwise-not tag-mask))
(define tag-shift    24)
(define tag-end      32)
(define untag-shift  -24)
(define char-shift   8)
(define char-tag     #b01001111)
(define char-tag-shifted (arithmetic-shift char-tag tag-shift))
(define bool-tag     (arithmetic-shift #b01011111 tag-shift))
(define true-tag     #b01011111)
(define true-v       (arithmetic-shift true-tag tag-shift))
(define false-tag    #b11011111)
(define false-v      (arithmetic-shift false-tag tag-shift))
(define empty-tag    #b01101111)
(define empty-list-v (arithmetic-shift empty-tag tag-shift))

(define (tag-bits v)
  (bitwise-bit-field v tag-shift tag-end))

(define (decode-immediate v)
  (if (= 0 (bitwise-and fixnum-mask v))
      v ; fixnum
      (let ([tag (tag-bits v)])
        (cond
         [(= tag true-tag) #t]
         [(= tag false-tag) #f]
         [(= tag empty-tag) empty]
         [(= tag char-tag) (integer->char (bitwise-and v no-tag-mask))]
         [else (raise-argument-error 'decode-immediate "immediate" v)]))))

(define (immediate-rep v)
  (cond
   [(integer? v) v]
   [(char? v) (bitwise-ior (char->integer v) char-tag-shifted)]
   [(boolean? v) (match v
                   [#t true-v]
                   [#f false-v])]
   [(empty? v) empty-list-v]
   [#t (raise-argument-error 'immediate-rep "immediate" v)]))

(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

(define (const? v)
  (or (integer? v)
      (char? v)
      (boolean? v)
      (empty? v)))

(define primitives
  '(+))

(define (prim? exp)
  (member exp primitives))

(define (primcall? v)
  (and (list? v)
       (member (primcall-op v) primitives)))

(define (primcall-op x)
  (car x))

(define (primcall-operand1 x)
  (cadr x))

(define (primcall-operand2 x)
  (caddr x))

(define (ref? exp)
  (symbol? exp))

;; (+ 1 27) => ((move (temp t1)
;;                    (const 1))
;;              (move (temp t2)
;;                    (const 27))
;;              (primcall + (temp t1) (temp t2)))
;;
;; (+ (+ 1 27) 49)
;;
;;   lw  0 t1 C1
;;   lw  0 t2 C2
;;   add t1 t2 t3
;;   lw  0 t4 C3
;;   add t3 t4 t5
;;
;;   add 0 t5 1 ;; return sequence
;;   jalr 6 7


;; <temp1> ::= (temp <t>)
;;
;; <mem1>  ::= (mem <a>)
;;
;; <exp1> ::= (const <v>)
;;         |  (primcall <prim> <t> ...)
;;         |  <temp1>
;;         |  <mem1>
;;
;; <stmt1> ::= (move <temp1> <exp1>)
;;          |  (move <mem1> <temp1>)
;;
;; <body1> ::= <stmt1> ... (return <temp1>)

; num-environments : natural
(define num-environments 0)

; environments : alist*[env-id,symbol]
(define environments '())

(struct env (id fields ntemp))

;; ; allocate-environment : list[symbol] -> env-id
;; (define (allocate-environment fields)
;;   (let ((id num-environments))
;;     (set! num-environments (+ 1 num-environments))
;;     (set! environments (cons (env id fields) environments))
;;     id))

;; ; get-environment : natural -> list[symbol]
;; (define (get-environment id)
;;   (cdr (assv id environments)))

(define num-temporaries 0)

(define (make-temp)
  (let ([t (list 'temp num-temporaries)])
    (set! num-temporaries (+ 1 num-temporaries))
    t))

(define (move? l)
  (tagged-list? 'move l))

(define (temp? l)
  (tagged-list? 'temp l))

(define (lambda? l)
  (tagged-list? 'lambda l))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (caddr exp))

(define (move->temp m)
  (findf temp? (cdr m)))

(define (body->ir1 e acc)
  (cons
   (match e
     
     [(? null?) (list 'return (move->temp (findf move? acc)))])
   acc))

(define (temp-for v)
  (match v
    [(? const?) (let ([t (make-temp)])
                  (list (list 'move t (list 'const v))
                        t))]))

(define (add-after-if e rest)
  (cond
   [(empty? e) rest]
   [(empty? rest) e]
   [else (list rest e)]))

(define (add-before-if e rest)
  (cond
   [(empty? e) rest]
   [(empty? rest) e]
   [else (list e rest)]))

;; (+ (+ 3 4) 7)
;; (+ 3 4)
;; 
;; ((move (temp 1) (const 3))
;;  (move (temp 2) (const 4)))
;;
;; ((move (temp 1) (const 3))
;;  (move (temp 2) (const 4))
;;  (primcall + (temp 1) (temp 2)))
;;
;;
;; t1 t2 t3 t4
;;
;; (tempify '(+ 4 3))
;; =>
;; '((move t1 3)
;;   (move t2 4)
;;   (move t3 (+ t1 t2)))
;; 't1
;;
;; (tempify '(+ (+ 4 3) 9))
;; =>
;; '((move t1 3)
;;   (move t2 4)
;;   (move t3 (+ t1 t2))
;;   (move t4 9)
;;   (move t5 (+ t3 t5))
;; 't5
;;
;; (+ (+ 4 3) 9)
;; > ((+ 4 3) 9)
;;  > (+ 4 3)
;;   > (4 3)
;;    > 4
;;    < (move t1 4) / t1
;;    > 3
;;    < (move t2 3) / t2
;;   < ((move t1 4) (move t2 3)) / (t1 t2)
;;  < ((move t1 4) (move t2 3) (move t3 (+ t1 t2))) / (t3)
;;  > (9)
;;  < (move t4 9) / t4
;; < (...) / (t3 t4)
;; < (... (move t5 (+ t3 t4))) / t5
;;
;; (tempify 3)
;; =>
;; '((move t1 3))
;; 't1

(define (tempify-args el)
  (match el
    [(list (? symbol?))
     (use-temps el)]
    [(list a tl ...)
     (let-values ([(s1    ref1) (use-temps a)]
                  [(stmts referents) (tempify-args tl)])
       (values (add-after-if s1 stmts)
               (add-after-if ref1 referents)))]
    [empty (values empty empty)]))

(define (tempify-exp el build)
  (let-values ([(stmts referents) (tempify-args el)])
    (let* ([end-expr (build referents)]
           [temp (make-temp)]
           [temp-move (list 'move temp end-expr)])
      (values (add-after-if temp-move stmts)
              temp))))

(define (use-temps e)
  (match e
    [(? primcall?)
     (tempify-exp (cdr e)
                  (lambda (tl)
                    (list 'primcall (car e) tl)))]
    [(? const?)
     (tempify-exp empty (lambda (l)
                          (list 'const e)))]
    [a
     (tempify-exp empty
                  (lambda (ref-l)
                    (match ref-l
                      [empty a])))]))

;; [(? lambda? exp)
;;  (list 'lambda
;;        (lambda->formals exp)
;;        (tempify-exp (lambda->exp exp)
;;                     (lambda (ref)
;;                       (list 'return ref))))]

;; (let-values ([(stmts tail-exp) (split-at-right exp 1)])
;;   (let ([mid (map linearize stmts)]
;;         [tail (linearize tail-exp)])
;;     (append 
;;      mid
;;      tail
;;      (list 'return (if (move? tail)
;;                        (move->temp tail)
;;                        (true-v))))))

;; [(list 'begin exp ...)
;;  (if (null? (cdr exp))
;;      (exp->ir1 exp)
;;      (let-values ([(stmts tail-exp) (split-at-right exp 1)])
;;        (list 'eseq
;;              (foldr (lambda (l r) (if (null? r) l (list 'seq l r)))
;;                     empty
;;                     (map stmt->ir1 stmts))
;;              (sexp->ir1 tail-exp))))]



;; [(? list?) (list 'eseq
;;                  (let-values ([(h t) (split-at-right e 1)])
;;                    (list 'eseq
;;                          (stm->ir1 h)
;;                          (list t))))]

;; (define (stm->ir1 e)
;;   (match e
;;     [(? list?))]))


(define (compile-program x)
  (let ([text (reverse *runtime-text*)]
        [data (reverse *runtime-data*)]
        [stack-idx -1])
    (define (emit s . args)
      (set! text (cons (apply format s args) text)))
    (define (emit-data s . args)
      (set! data (cons (apply format s args) data)))
    (define (save-reg n)
      (emit "    sw    ~a ~a ~a" sp-reg n stack-idx)
      (set! stack-idx (sub1 stack-idx)))
    (define (emit-expr x)
      (cond
       [(const? x)
        (emit "    lw   0 1 Cn")
        (emit-data "Cn    .fill ~s" (immediate-rep x))]
       [(primcall? x)
        (match (primcall-op x)
          [+
           (emit-expr (primcall-operand2 x))
           (emit-expr (primcall-operand1 x))
           ;;(emit "    add ~a ~a ~a" c1-reg rv-reg rv-reg)
           ])]
       [else (raise-argument-error 'emit-expr "expr" x)]))
    ;; do stuff here
    (emit-expr x)
    (emit "    jalr 5 6")
    
    ;; write output
    (for ([s (reverse text)])
      (displayln s))
    (for ([s (reverse data)])
      (displayln s))))

(define (compile-to x path)
  (with-output-to-file path
    (lambda ()
      (compile-program x))
    #:exists 'must-truncate))
