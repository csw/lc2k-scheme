#lang racket

(require racket/dict)
(require racket/format)

(provide compile-program compile-to *lc2k-rv* decode-immediate)

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

(struct env (id dict parent))

; environments : alist*[env-id,symbol]
(define environments empty)

(define (make-env parent)
  (let* ([env-id num-environments]
         [e (env num-environments (make-hash) parent)])
    (set! num-environments (add1 num-environments))
    (set! environments (dict-set environments env-id e))))

(define (env-define env k v)
  (dict-set! (env-dict env) k v))

(define (env-lookup env k)
  (if (env-parent env)
      (dict-ref (env-dict env) k
                (lambda ()
                  (env-lookup (env-parent env))))
      (dict-ref (env-dict env) k)))

;; global
(make-env #f)

(define lambda-n 0)

(define (lambda-label)
  (begin0
      (format "L~a" lambda-n)
    (set! lambda-n (add1 lambda-n))))

(define constants (make-hash))
(define constant-n 0)

(define (global-env) (dict-ref environments 0))

(define (const-ref val)
  (if (hash-has-key? constants val)
      (hash-ref constants val)
      (let ([cname (format "C~a" constant-n)])
        (hash-set! constants val cname)
        (set! constant-n (add1 constant-n))
        cname)))

(define (write-constant-defs)
  (hash-for-each constants
                 (lambda (imm cname)
                   (displayln (string-join (list (~a cname
                                                     #:max-width 6
                                                     #:min-width 6)
                                                 ".fill "
                                                 (~a imm))
                                           "  ")))))

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
   [else (cons e rest)]))

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

(define (reg-ref n)
  (list 'register n))

(define reg-n (make-parameter 0))

(define (tempify-args el env)
  (begin0 (for/fold ([stmts empty] [refs empty])
              ([a el])
            (let-values ([(s1 ref1) (use-temps-inner a env)])
              (values
               (cond
                [(and (empty? stmts)
                      (symbol? (car s1))) (list s1)]
                [(empty? stmts) s1]
                [else (cons s1 stmts)])
               (cons ref1 refs))))
    (reg-n (- (reg-n) (length el)))))

(define (tempify-exp el env build)
  (let-values ([(stmts referents) (tempify-args el env)])
    (reg-n (add1 (reg-n)))
    (let* ([end-expr (build referents)]
           [reg (reg-ref (reg-n))]
           [temp-move (list 'move reg end-expr)])
      (values (add-before-if temp-move stmts)
              reg))))

(define (pasm-lambda-label l)
  (cadr (assq 'label (lambda->exp l))))

(define (use-temps-inner e env)
  (match e
    [(? const?)
     (tempify-exp empty env (lambda (l)
                              (list 'const e)))]
    [(? primcall?)
     (tempify-exp (cdr e)
                  env
                  (lambda (tl)
                    (append (list 'primcall (car e)) tl)))]
    [(? lambda? exp)
     (let ([inner-env (make-env env)])
       (list 'lambda
             (lambda->formals exp)
             (let-values ([(stmts referent)
                           (use-temps-inner (lambda->exp exp) inner-env)])
               (cons (list 'label (lambda-label))
                     (reverse (cons (list 'return referent)
                                    (if (list? (car stmts))
                                        stmts
                                        (list stmts))))))))]
    [(list 'define name (? lambda? l))
     (let* ([ir (use-temps-inner l env)]
            [label (pasm-lambda-label ir)])
       (env-define env name (list (cons 'type 'lambda)
                                  (cons 'def l)
                                  (cons 'formals (lambda->formals l))
                                  (cons 'label label)))
       ir)]))

(define (use-temps e [env (global-env)])
  (parameterize ([reg-n 0])
    (use-temps-inner e env)))

(define no-label "      ")

(define insn-fmt "")

(define (code-gen pasm-lambda)
  (let ([pending-label no-label])
    (define emit
      (lambda args
        (begin0 (string-join (map (lambda (v)
                                    (~a v #:min-width 7))
                                  (cons pending-label args)))
          (set! pending-label no-label))))
    (let ([raw-code (for/list ([expr (lambda->exp pasm-lambda)])
                      (match expr
                        [(list 'label l)
                         (set! pending-label (~a l
                                                 #:min-width 6
                                                 #:max-width 6))]
                        [(list 'move (list 'register dest) rvalue)
                         (match rvalue
                           [(list 'const v)
                            (let* ([imm (immediate-rep v)]
                                   [cname (const-ref imm)])
                              (emit "lw" 0 dest cname))]
                           [(list 'primcall
                                  '+
                                  (list 'register a)
                                  (list 'register b))
                            (emit "add" a b dest)])]
                        [(list 'return (list 'register last-reg))
                         (let ([ret (emit "jalr" 5 6)])
                           (if (= last-reg 1)
                               ret
                               (list (emit "add" 0 last-reg 1)
                                     ret)))]))])
      (flatten (filter string? raw-code)))))

;; (match el
;;   [(list (? symbol?))
;;    (use-temps el)]
;;   [(list a tl ...)
;;    (let-values ([(s1    ref1) (use-temps a)]
;;                 [(stmts referents) (tempify-args tl)])
;;      (values (add-before-if s1 stmts)
;;              (add-before-if ref1 referents)))]
;;   [empty (values empty empty)]))

;; [a
;;  (tempify-exp empty
;;               (lambda (ref-l)
;;                 (match ref-l
;;                   [empty a])))]

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

(define *runtime-text*
  '("        lw   0 6 entry"
    "        lw   0 7 stack"
    "        jalr 6 5"
    "        sw   0 1 SCMrv"
    "SCMh    halt"))

(define (runtime-data entry-pt-label)
  (list "stack   .fill 65535"
        "SCMrv   .fill 559038737"
        (format "entry   .fill ~a" entry-pt-label)))

(define (compile-program prog)
  (for-each displayln *runtime-text*)
  (let* ([pasm (use-temps prog)]
         [entry-label (pasm-lambda-label pasm)])
    (for-each displayln (code-gen pasm))
    (for-each displayln (runtime-data entry-label)))
  (write-constant-defs))

;; (define (compile-program-orig x)
;;   (let ([text (reverse *runtime-text*)]
;;         [data (reverse *runtime-data*)]
;;         [stack-idx -1])
;;     (define (emit s . args)
;;       (set! text (cons (apply format s args) text)))
;;     (define (emit-data s . args)
;;       (set! data (cons (apply format s args) data)))
;;     (define (save-reg n)
;;       (emit "    sw    ~a ~a ~a" sp-reg n stack-idx)
;;       (set! stack-idx (sub1 stack-idx)))
;;     (define (emit-expr x)
;;       (cond
;;        [(const? x)
;;         (emit "    lw   0 1 Cn")
;;         (emit-data "Cn    .fill ~s" (immediate-rep x))]
;;        [(primcall? x)
;;         (match (primcall-op x)
;;           [+
;;            (emit-expr (primcall-operand2 x))
;;            (emit-expr (primcall-operand1 x))
;;            ;;(emit "    add ~a ~a ~a" c1-reg rv-reg rv-reg)
;;            ])]
;;        [else (raise-argument-error 'emit-expr "expr" x)]))
;;     ;; do stuff here
;;     (emit-expr x)
;;     (emit "    jalr 5 6")
    
;;     ;; write output
;;     (for ([s (reverse text)])
;;       (displayln s))
;;     (for ([s (reverse data)])
;;       (displayln s))))

(define (compile-to x path)
  (with-output-to-file path
    (lambda ()
      (compile-program x))
    #:exists 'must-truncate))
