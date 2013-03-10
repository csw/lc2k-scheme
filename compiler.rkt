#lang racket

(require racket/dict)
(require racket/format)
(require racket/trace)

(provide compile-program compile-to *lc2k-rv* decode-immediate)

(define sp-reg 7)
(define call-reg 5)
(define ret-reg 6)
(define rv-reg 1)

(define *lc2k-rv* "SCMrv")

(define tagged-mask  #b11000000000000000000000000000000)
(define tagged-tag   #b01000000000000000000000000000000)
(define tag-mask     #xEF000000)
(define no-tag-mask  (bitwise-not tag-mask))
(define tag-shift    24)
(define tag-end      32)
(define untag-shift  -24)
(define char-shift   8)
(define char-tag     (arithmetic-shift #b01001111 tag-shift))
(define bool-tag     (arithmetic-shift #b01011111 tag-shift))
(define true-v       (add1 bool-tag))
(define false-v      bool-tag)
(define empty-tag    #b01101111)
(define empty-list-v (arithmetic-shift empty-tag tag-shift))
(define pointer-mask #x0000FFFF)
(define cons-tag     (arithmetic-shift #b01000111 tag-shift))

(define (tag-bits-old v)
  (bitwise-bit-field v tag-shift tag-end))

(define (tag-bits v)
  (bitwise-and v tag-mask))

(define (decode-immediate v)
  (if (= tagged-tag (bitwise-and tagged-mask v))
      (let ([tag (tag-bits v)])
        (cond
         [(= v true-v) #t]
         [(= v false-v) #f]
         [(= v empty-list-v) empty]
         [(= tag char-tag) (integer->char (bitwise-and v no-tag-mask))]
         [else (raise-argument-error 'decode-immediate "immediate" v)]))
      ;; fixnum
      v))

(define (immediate-rep v)
  (cond
   [(integer? v) v]
   [(char? v) (bitwise-ior (char->integer v) char-tag)]
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
  '(+ %nand %and %eq? %zero?))

(define prim-predicates
  '(%eq? %zero?))

(define (prim? exp)
  (member exp primitives))

(define (prim-predicate? exp)
  (member exp prim-predicates))

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

                                        ; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

                                        ; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

                                        ; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

                                        ; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))
(define (app? exp)
  (pair? exp))
                                        ; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

                                        ; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))

(define (expand-primcall exp)
  (let ([expanded
         (match exp
           [(list 'empty? v)        `(%eq? ,(expand-prims v) empty)]
           [(list 'pair? v)         `(%tagged? list-tag ,(expand-prims v))]
           [(list '%tagged? tag v)  `(%zero? (%band tag ,(expand-prims v)))]
           [(list '%ptr v)          `(%and ,(expand-prims v) pointer-mask)]
           ;[(list '%car v)          `(%load (%ptr ,(expand-prims v)) 0)]
           ;[(list '%cdr v)          `(%load (%ptr ,(expand-prims v)) 1)]
           [(list 'zero? v)         `(%zero? ,(expand-prims v))]
           ;; simple but unsafe versions of common functions
           ;[(list 'car v)           `(%car ,(expand-prims v))]
           ;[(list 'cdr v)           `(%cdr ,(expand-prims v))]
           [(list '+ x y)           `(%add ,(expand-prims x)
                                           ,(expand-prims y))]
           [(list 'bitwise-and x y) `(%band ,(expand-prims x)
                                            ,(expand-prims y))]
           [(list 'bitwise-or x y) `(%nand ,(expand-prims `(bitwise-not ,x))
                                           ,(expand-prims `(bitwise-not ,y)))]
           [(list 'bitwise-not x)  `(%bnot ,(expand-prims x))]
           [else #f])])
    (if expanded
        (cons 'primcall (or (expand-primcall expanded) expanded))
        #f)))

(define (expand-call exp)
  (or (expand-primcall exp)
      (cons 'labelcall
            (cons (car exp)
                  (map expand-prims (cdr exp))))))

(define (expand-if-pred exp)
  (let ([expanded (expand-prims exp)])
    (match expanded
      [(list-rest (? prim-predicate?) ... args) expanded]
      ;; XXX: review this
      [else `(%eq? #t ,expanded)])))

(define (expand-prims exp)
  (cond
   ; Core forms:
   ((tagged-list? 'code exp) `(code ,(cadr exp) ,(expand-prims (caddr exp))))
   ((const? exp)      exp)
   ((prim? exp)       exp)
   ((ref? exp)        exp)
   ;;((lambda? exp)     `(lambda ,(lambda->formals exp)
   ;;                      ,(expand-prims (lambda->exp exp))))
   ;((set!? exp)       `(set! ,(set!->var exp) ,(set!->exp exp)))
   ((if? exp)         `(if ,(expand-if-pred (if->condition exp))
                           ,(expand-prims (if->then exp))
                           ,(expand-prims (if->else exp))))
   
                                        ; Sugar:
   ;;((let? exp)        (expand-prims (let=>lambda exp)))
   ;;((letrec? exp)     (expand-prims (letrec=>lets+sets exp)))
   ;;((begin? exp)      (expand-prims (begin=>let exp)))
   
                                        ; Applications:
   ((app? exp)        (expand-call exp))
   (else              (error "unknown exp: " exp))))

; num-environments : natural
(define num-environments 0)

(struct env (id dict parent))

; environments : alist*[env-id,symbol]
(define environments empty)

(define (make-env parent)
  (let* ([env-id num-environments]
         [e (env num-environments (make-hash) parent)])
    (set! num-environments (add1 num-environments))
    (set! environments (dict-set environments env-id e))
    e))

(define (env-define env k v)
  (dict-set! (env-dict env) k v))

(define (env-lookup env k)
  (if (env-parent env)
      (dict-ref (env-dict env) k
                (lambda ()
                  (env-lookup (env-parent env) k)))
      (dict-ref (env-dict env) k)))

;; global

(make-env #f)
(define (global-env) (dict-ref environments 0))

(define lambda-n 0)

(define (lambda-label)
  (begin0
      (format "L~a" lambda-n)
    (set! lambda-n (add1 lambda-n))))

(define (lambda-addr-label l)
  (string-append "A" (substring l 1)))

(define internal-n 0)

(define (internal-label)
  (begin0
      (format "I~a" internal-n)
    (set! internal-n (add1 internal-n))))

(define constants (make-hash))
(define constant-n 0)

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

(define (init-global-env)
  (set! constants (make-hash))
  (set! constant-n 0)
  (for ([def `((empty . (immediate ,empty-list-v))
               (#t . (immediate ,true-v))
               (#f . (immediate ,false-v)))])
    (env-define (global-env)
                (car def)
                (cdr def))))


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

(define (reg-ref n)
  (list 'register n))

(define reg-n (make-parameter 0))

(define (sethi-ullman-label code-exp)
  (match-define (list 'code (list formals ...) body-exp)
                code-exp)
  (let* ([need-table (make-hash)]
         [need (lambda (exp)
                 (hash-ref need-table exp))]
         [need! (lambda (exp n)
                  (hash-set! need-table exp n))])
    (define (su-scan exp)
      (need! exp
             (match exp
               [(? const?) ; 1 register to load the constant
                1]
               [(? symbol?)
                (if (memq exp formals)
                    0
                    1)]
               [(list 'primcall prim arg)
                (su-scan arg)
                (if (memq prim prim-predicates)
                    (need arg)
                    (max 1 (need arg)))]
               [(list 'primcall prim arg1 arg2)
                (for-each su-scan (cdr exp))
                (if (= (need arg1) (need arg2))
                    (add1 (need arg1))
                    (apply max 1 (map need (cdr exp))))]
               ;; function calls
               ;; XXX: this may be totally bogus
               [(list 'labelcall fun args ...)
                (for-each su-scan args)
                (max 1 (foldl + 0 (map (lambda (arg)
                                         (min 1 (need arg)))
                                       args)))]
               [(list 'if if-pred if-then if-else) ;; sequentially
                (match if-pred
                  [(list 'primcall '%zero? pa0) ;; %zero v
                   (su-scan pa0)
                   (need pa0)]
                  [(list 'primcall '%eq? pa0 pa1)
                   (for-each su-scan (list pa0 pa1))
                   (apply max 1 (map need (list pa0 pa1)))])])))
    (su-scan body-exp)
    need-table))

(define (spill-code reg)
  (match reg
    [(list 'register n)
     `((sw ,sp-reg ,n ,(+ n 1)
           ,(format " ; save r~a" n)))]))

(define (unspill-code reg)
  (match reg
    [(list 'register n)
     `((lw ,sp-reg ,n ,(+ n 1)
           ,(format " ; restore r~a" n)))]))

;; simple stack frame handling for now
(define *frame-size* 16)

(define *debug-codegen* #f)

(define (sethi-ullman-gen code-exp entry-label)
  (match-define (list 'code (list formals ...) body-exp)
                code-exp)
  (let* ([env (make-env (global-env))]
         [need-table (sethi-ullman-label code-exp)]
         [need (lambda (exp)
                 (hash-ref need-table exp 0))]
         [exp-reg-table (make-hash)]
         [exp-reg (lambda (exp) (hash-ref exp-reg-table exp))]
         [insns empty]
         [begin-label (internal-label)]
         [n 0]
         [k 3])
    (for ([arg formals]
          [reg (in-naturals 1)])
      (env-define env arg (list 'register reg)))
    (define (emit! . args)
      (if (list? (car args))
          (apply emit! (car args))
          (set! insns (cons args insns))))
    (define (emit-all! . l)
      (for-each emit! l))
    (define (cg exp dd cd next-label)
      (define (set-n! new-n)
        (when (< new-n 0)
          (error 'set-n! "Illegal new register state, n=~a" new-n))
        (set! n new-n))
      (let ([choose-reg
             (lambda (delta (use-dd #t))
               (unless (zero? delta)
                 (set-n! (+ n delta)))
               (let ([reg (if (and dd use-dd)
                              dd
                              (begin
                                (when (<= n 0)
                                  (error 'choose-reg
                                         "illegal register state, n=~a" n))
                                (list 'register (- 6 n))))])
                 (hash-set! exp-reg-table exp reg)
                 reg))]
            [gen-tail
             (lambda ()
               (cond
                ;; expression in tail position, return
                [(eq? cd 'return)
                 ;; increment stack pointer
                 (emit! 'lw 0 call-reg (const-ref *frame-size*))
                 (emit! 'add sp-reg call-reg sp-reg
                        (format "; SP += ~a" *frame-size*))
                 ;; and return
                 (emit! 'jalr ret-reg call-reg "; return")]
                ;; continue directly to next label
                [(eq? cd next-label) #t]
                ;; jump to next label
                [else (emit! 'beq 0 0 cd)]))]
            [gen-children
             (lambda (cl cr body-label)
               (cond
                ;; both already in registers
                [(and (= (need cl) 0) (= (need cr) 0))
                 (list (env-lookup env cl) (env-lookup env cr))]
                ;; both children need >= k registers
                [(and (>= (need cl) k) (>= (need cr) k))
                 (let* ([spill-label (internal-label)]
                        [unspill-label (internal-label)]
                        [r-reg (cg cr #f spill-label spill-label)])
                   (set-n! (- n 1))
                   (emit! 'label spill-label)
                   (emit-all! (spill-code r-reg))
                   (let ([l-reg (cg cl #f unspill-label unspill-label)])
                     (emit! 'label unspill-label)
                     (emit-all! (unspill-code r-reg))
                     (list l-reg r-reg)))]
                ;; one doesn't, evaluate right one first
                [(>= (need cl) (need cr))
                 (let* ([r-label (internal-label)]
                        [l-reg (cg cl #f r-label r-label)])
                   (emit! 'label r-label)
                   (begin0
                       (list l-reg
                             (cg cr #f body-label body-label))
                     (set-n! (- n 1))))]
                ;; left one first
                [else
                 (let* ([l-label (internal-label)]
                        [r-reg (cg cr #f l-label l-label)])
                   (emit! 'label l-label)
                   (begin0
                       (list (cg cl #f body-label body-label)
                             r-reg)
                     (set-n! (- n 1))))]))])
        (when *debug-codegen*
          (trace choose-reg)
          (trace set-n!))
        (match exp
          ;; constant reference
          [(? const?)
           (let* ([imm (immediate-rep exp)]
                  [cname (const-ref imm)]
                  [reg (choose-reg 1)])
             (emit! 'lw 0 reg cname
                    (format "; r~a = ~a" (cadr reg) exp))
             (gen-tail)
             reg)]
          ;; symbol
          [(? symbol?)
           (let* ([referent (env-lookup env exp)])
             (match referent
               ;; constant, from the constant pool
               [(list 'immediate val)
                (let* ([cname (const-ref val)]
                       [reg (choose-reg 1)])
                  (emit! 'lw 0 reg cname
                         (format "; r~a = ~a" (cadr reg) exp))
                  (gen-tail)
                  reg)]
               ;; register variable (formal param)
               [(list 'register num)
                (gen-tail)
                referent]))]
          ;; unary primitive
          [(list 'primcall prim arg)
           (let ([call-label (internal-label)])
             (let ([arg-reg (cg arg #f call-label call-label)]
                   [dest-reg (choose-reg 0)])
               (match prim
                 ['%bnot (emit! 'nand arg-reg arg-reg dest-reg)]
                 ['%car #f] ;; TODO
                 ['%cdr #f])
               (gen-tail)
               dest-reg))]
          ;; binary primitive
          [(list 'primcall prim arg1 arg2)
           (let* ([call-label (internal-label)]
                  [child-regs (gen-children arg1 arg2 call-label)]
                  [t1 (car child-regs)]
                  [t2 (cadr child-regs)]
                  [dest-reg (choose-reg 0)]) ;; decr. in gen-children
             (match prim
               ['%add  (emit! 'add t1 t2 dest-reg)]
               ['%nand (emit! 'nand t1 t2 dest-reg)]
               ['%band (emit-all!
                        `((nand ,t1 ,t2 ,dest-reg)
                          (nand ,dest-reg ,dest-reg ,dest-reg)))])
             (gen-tail)
             dest-reg)]
          ;; function call
          [(list 'labelcall sym args ...)
           ;; save our formals onto the stack
           (for ([reg (in-range 1 (max (length args)
                                       (length formals)))])
             (emit-all! (spill-code (list 'register reg))))
           ;; save live temporaries
           (for ([reg (in-range 1 (add1 n))])
             (emit-all! (spill-code (list 'register (- 6 reg)))))
           ;; marshal arguments
           (for ([arg args]
                 [reg (in-naturals 1)])
             (let ([arg-done-label (internal-label)])
               (cg arg (list 'register reg) arg-done-label arg-done-label)
               ;; XXX: need to mark this variable to be loaded from
               ;; the stack now
               (emit! 'label arg-done-label)))
           ;; XXX: not sure if this is the right way to handle n...
           (let ([dest-reg (choose-reg (if (> (length args) 1)
                                           0
                                           1)
                                       #f)])
             ;; TODO: check for tail call
             (emit! 'lw 0 dest-reg (lambda-addr-label
                                    (cadr (env-lookup env sym))))
             ;; save our return addr
             (emit-all! (spill-code (list 'register ret-reg)))
             ;; perform the call
             (emit! 'jalr dest-reg ret-reg (format "; call ~a" sym))
             ;; restore the return addr
             (emit-all! (unspill-code (list 'register ret-reg)))
             ;; put the return value where it belongs
             (emit! 'add 0 1 dest-reg)
             ;;(set-n! (- n 1))
             ;; restore temporaries
             (for ([reg (in-range 1 n)])
               (emit-all! (unspill-code (list 'register (- 6 reg)))))
             ;; XXX: lazily restore formals
             (for ([reg (in-range 1 (max (length args)
                                         (length formals)))])
               (emit-all! (unspill-code (list 'register reg))))
             (gen-tail)
             dest-reg)]
          ;; conditional
          [(list 'if if-pred if-then if-else)
           (let* ([true-label (internal-label)]
                  [false-label (internal-label)]
                  [branch-label (internal-label)]
                  [registers
                   (match if-pred
                     [(list 'primcall '%zero? pa0) ;; %zero v
                      (list '(register 0)
                            (cg pa0 #f branch-label branch-label))]
                     [(list 'primcall '%eq? pa0 pa1)
                      (gen-children pa0 pa1 branch-label)])])
             (emit! 'label branch-label)
             (emit! 'beq (car registers) (cadr registers) true-label)
             (let ([base-n (- n (need if-pred))])
               (set-n! base-n)
               (emit! 'label false-label)
               (cg if-else dd cd true-label)
               (set-n! base-n)
               (emit! 'label true-label)
               (cg if-then dd cd next-label)))])))
    (when *debug-codegen*
      (trace cg)
      (eprintf "need: ~a\n" (pretty-format need-table)))
    (emit! 'noop (format "; ~v" code-exp))
    ;; function entry point
    (emit! 'label entry-label)
    ;; decrement stack pointer
    (emit! 'lw 0 call-reg (const-ref (- *frame-size*)))
    (emit! 'add sp-reg call-reg sp-reg
           (format "; SP -= ~a" *frame-size*))
    ;; begin-label
    (emit! 'label begin-label)
    (cg body-exp '(register 1) 'return #f)
    (reverse insns)))

(define no-label "      ")

(define (gen-asm sasm)
  (let* ([pending-label no-label]
         [unwrap (lambda (v)
                   (match v
                     [(list 'register n) n]
                     [else v]))]
         [fmt-asm
          (lambda (dir)
            (begin0 (string-join (map (lambda (v)
                                        (~a v #:min-width 7))
                                      (cons pending-label
                                            (map unwrap dir))))
              (set! pending-label no-label)))]
         [handle-directive
          (lambda (dir)
            (match dir
              [(list 'label (? string? label))
               (set! pending-label label)
               #f]
              [(? list?) (fmt-asm dir)]))])
    (filter identity (map handle-directive sasm))))


(define *runtime-text*
  '("        lw   0 5 entry"
    "        lw   0 7 stack"
    "        jalr 5 6"
    "        sw   0 1 SCMrv"
    "SCMh    halt"
    "        noop"
    "Lcons   lw   0 5 heap"
    "        lw   0 4 consS"
    "        add  4 5 4"
    "        sw   0 4 heap"
    "        sw   4 1 0"
    "        sw   4 2 1"
    "        lw   0 5 ctag"
    "        add  4 5 1"
    "        jalr 6 5"
    "Lcar    lw   0 5 pmask"
    "        nand 1 5 1"
    "        nand 1 1 1"
    "        lw   1 1 0"
    "        jalr 6 5"
    "Lcdr    lw   0 5 pmask"
    "        nand 1 5 1"
    "        nand 1 1 1"
    "        lw   1 1 1"
    "        jalr 6 5"))

(define (runtime-data entry-pt-label)
  (list "stack   .fill 65535"
        "heapS   .fill 8192"
        "heap    .fill 8192"
        "SCMrv   .fill 559038737"
        "consS   .fill 2"
        (format "entry   .fill ~a" entry-pt-label)
        (format "ctag    .fill ~a" cons-tag)
        (format "pmask   .fill ~a" pointer-mask)))

(define *asm-functions*
  '((cons . "Lcons")
    (car  . "Lcar")
    (cdr  . "Lcdr")))

(define (compile-top prog)
  (init-global-env)
  (for ([fdef *asm-functions*])
    (env-define (global-env)
                (car fdef)
                (list 'fun-label (cdr fdef))))
  (define (compile-fun code label)
    (for-each displayln
              (gen-asm (sethi-ullman-gen (expand-prims code)
                                         label))))
  (for-each displayln *runtime-text*)
  (match prog
    [(list 'labels
           (list (list lvars lexprs) ...)
           top-expr)
     (for ([lvar lvars]
           [lexpr lexprs])
       (let ([label (lambda-label)])
         (env-define (global-env) lvar (list 'fun-label label))
         (compile-fun lexpr label)))
     (let ([entry-label (lambda-label)])
       (compile-fun `(code () ,top-expr) entry-label)
       (for-each displayln (runtime-data entry-label)))])
  (write-constant-defs)
  (for ([label-entry (in-hash-values (env-dict (global-env)))]
        #:when (tagged-list? 'fun-label label-entry))
    (let ([label-name (cadr label-entry)])
      (displayln (format "~a    .fill ~a"
                         (lambda-addr-label label-name)
                         label-name)))))

;; convert to our labels / code representation
(define (transform-program prog)
  (let*-values ([(defs rest) (partition (lambda (e)
                                          (tagged-list? 'define e))
                                        (if (list? prog)
                                            prog
                                            (list prog)))]
                [(label-defs) (for/list ([def defs])
                                (match def
                                  [(list 'define
                                         (list name formals ...)
                                         body)
                                   `(,name (code ,formals ,body))]))]
                [(toplevel) (if (and (pair? rest) (empty? (cdr rest)))
                                (car rest)
                                rest)])
    `(labels ,label-defs ,toplevel)))

(define (compile-program prog)
  (compile-top (transform-program prog)))

(define (compile-to x path)
  (with-output-to-file path
    (lambda ()
      (compile-program x))
    #:exists 'must-truncate))
