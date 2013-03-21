#lang racket

;; compiler.rkt
;; Scheme compiler for the LC2K
;;
;; Clayton Wheeler <cswh@umich.edu>, 2013

(require racket/dict)
(require racket/format)
(require racket/trace)

(provide compile-program compile-to *lc2k-rv* decode-immediate)

;; The LC2K has 8 registers, used as follows.
;; 
;; r0: always 0
;; r1: return value, first argument
;; r2: second argument
;; r3: third argument
;; r4: scratch
;; r5: scratch
;; r6: return address
;; r7: stack pointer
;;
;; The arguments and return address may be spilled to the stack and
;; their registers reused.
;;
;; r4 and r5 are reserved for loading and storing spilled variables.

(define sp-reg 7)
(define call-reg 5)
(define spill-reg 5)
(define ret-reg 6)
(define rv-reg 1)

(define *lc2k-rv* "SCMrv")

;;;; Type tags

;; Two high bits are reserved to distinguish fixnums from other
;; types. A word whose two most significant bits are 01 is an
;; immediate Scheme value or a tagged pointer.
;;
;; Immediates:
;;   fixnums, -2^31 to 2^30 - 1
;;   booleans: #t and #f
;;   chars
;;   The empty list: empty
;;
;; Pointer types:
;;   conses
;;
;; Unimplemented:
;;   functions
;;   vectors
;;   symbols
;;   strings
;;
;; bits 31-30: 01 for tagged Scheme types, otherwise fixnum
;; bit  29:    pointer (1) or immediate (0)
;; bits 28-25: type tag for immediate or pointer
;;
;; immediate type tags (29-25):
;; 00000 char
;; 00001 bool
;; 00010 empty
;;
;; pointer type tags (29-25)
;; 10000 cons
;; 10001 vector
;; 10010 string
;; 10011 symbol
;; 10100 closure
;;
;; For pointers:
;;   bit 24: constant pointer

(define (type-tag bits)
  (bitwise-ior tagged-tag
               (arithmetic-shift bits tag-shift)))

(define (immed-type-tag bits)
  (type-tag bits))

(define (pointer-type-tag bits)
  (type-tag (bitwise-ior (expt 2 5) bits)))


(define tagged-mask  #b11000000000000000000000000000000)
(define tagged-tag   #b01000000000000000000000000000000)
(define type-tag-mask #xFE000000)
(define no-type-tag-mask (bitwise-not type-tag-mask))
(define tag-shift    25)
(define char-tag     (immed-type-tag #b0000))
(define bool-tag     (immed-type-tag #b0001))
(define true-v       (add1 bool-tag))
(define false-v      bool-tag)
(define empty-tag    (immed-type-tag #b0010))
(define empty-list-v empty-tag)
(define pointer-mask #x0000FFFF)
(define cons-tag     (pointer-type-tag #b0000))

(define (tag-bits v)
  (bitwise-and v type-tag-mask))

(define (decode-immediate v)
  (if (= tagged-tag
         (bitwise-and tagged-mask v))
      (let ([tag (tag-bits v)])
        (cond
         [(= v true-v) #t]
         [(= v false-v) #f]
         [(= v empty-list-v) empty]
         [(= tag char-tag) (integer->char (bitwise-and v no-type-tag-mask))]
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

;; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

;; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

;; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

;; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))
(define (app? exp)
  (pair? exp))

;; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

;; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))

(define (expand-primcall exp)
  (let ([expanded
         (match exp
           [(list '%tagged? mask tag v)
            `(%eq? (primcall %band
                             ,(expand-prims mask)
                             ,(expand-prims v))
                   ,(expand-prims tag))]
           [(list '= x y)           `(%eq? ,(expand-prims x)
                                           ,(expand-prims y))]
           [(list '+ x y)           `(%add ,(expand-prims x)
                                           ,(expand-prims y))]
           [(list '- x)             `(%add 1
                                           ,(expand-prims
                                             (list 'bitwise-not x)))]
           [(list '- x (? number? y))      `(%add ,(expand-prims x)
                                           ,(- y))]
           [(list '- x y)           `(%add ,(expand-prims x)
                                           ,(expand-prims
                                             (list '- y)))]
           [(list 'bitwise-and x y) `(%band ,(expand-prims x)
                                            ,(expand-prims y))]
           [(list 'bitwise-ior x y) `(%nand ,(expand-prims `(bitwise-not ,x))
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
  (match exp
    [(list (? prim-predicate? pred) args ...)
     (list* 'primcall pred (map expand-prims args))]
    [(list 'empty? v)
     (list 'primcall '%eq? (expand-prims v) empty)]
    ;; and, or
    [(list (and op (or 'and 'or)) args ...)
     (list* op (map expand-if-pred args))]
    [else
     ;; XXX: review all this
     (let ([expanded (expand-prims exp)])
       (match expanded
         ;; eq? or zero?
         [(list-rest 'primcall (? prim-predicate?) ... args) expanded]
         [else `(not (primcall %eq? #f ,expanded))]))]))

(define (expand-prims exp)
  (match exp
   ;; Core forms:
   [(list 'code (list args ...) body-exprs ...)
    `(code ,args ,@(map expand-prims body-exprs))]
   [(? const?)      exp]
   [(? prim?)       exp]
   [(? ref?)        exp]
   ;;((lambda? exp)     `(lambda ,(lambda->formals exp)
   ;;                      ,(expand-prims (lambda->exp exp))))
   ;((set!? exp)       `(set! ,(set!->var exp) ,(set!->exp exp)))
   [(? if?)         `(if ,(expand-if-pred (if->condition exp))
                           ,(expand-prims (if->then exp))
                           ,(expand-prims (if->else exp)))]
   [(list (and (or 'and 'or)
               op)
          args ..1)         `(,op ,@(map expand-prims args))]
                                        ; Sugar:
   ;;((let? exp)        (expand-prims (let=>lambda exp)))
   ;;((letrec? exp)     (expand-prims (letrec=>lets+sets exp)))
   ;;((begin? exp)      (expand-prims (begin=>let exp)))
   
                                        ; Applications:
   [(? app?)        (expand-call exp)]
   [else              (error "unknown exp: " exp)]))

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

(define *asm-functions*
  '((cons . "Lcons")
    (car  . "Lcar")
    (cdr  . "Lcdr")))

(define immed-constants
  `((empty          . ,empty-list-v)
    (#t             . ,true-v)
    (#f             . ,false-v)
    (%tagged-mask   . ,tagged-mask)
    (%tagged-tag    . ,tagged-tag)
    (%type-tag-mask . ,type-tag-mask)
    (%char-tag      . ,char-tag)
    (%bool-tag      . ,bool-tag)
    (%cons-tag      . ,cons-tag)
    (%sign-bit      . ,(expt 2 31))))

(define (init-global-env)
  (set! num-environments 0)
  (set! environments empty)
  (make-env #f)
  (set! constants (make-hash))
  (set! constant-n 0)
  (set! lambda-n 0)
  (set! internal-n 0)
  (for ([def immed-constants])
    (env-define (global-env)
                (car def)
                (list 'immediate (cdr def))))
  (for ([fdef *asm-functions*])
    (env-define (global-env)
                (car fdef)
                (list 'fun-label (cdr fdef)))))
(init-global-env)


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

(define reg0 (reg-ref 0))

(define reg-n (make-parameter 0))

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

(define (code-formals code)
  (cadr code))


(define (gen-pred-code pred true-label false-label next-label)
  ;;
  ;; (gen-sequence conditions init-continue targets): return a code
  ;;     list for a list of conditional expressions.
  ;;
  ;; conditions: list of conditional expressions.
  ;;
  ;; targets: a function which, given the following label in sequence,
  ;;     or #f for end-of sequence, will return two values, the true
  ;;     and false branch targets.  For (and ...) these should be
  ;;     (following false-label); for (or ...) they should be
  ;;     (true-label following).
  ;;
  (define (gen-sequence conditions targets)
    (let-values ([(labels fol code)
                  (for/fold ([following   false-label]
                             [cond-after  #f]
                             [code-after  empty])
                      ([cond (reverse conditions)])
                    (let*-values
                        ([(cond-label) (internal-label)]
                         [(true-l false-l) (targets cond-after)]
                         [(cond-code) (gen-pred-code cond
                                                     true-l
                                                     false-l
                                                     following)])
                      (values cond-label
                              cond-label
                              (append (list (list 'label cond-label))
                                      cond-code
                                      code-after))))])
      (cdr code)))
  (match pred
    [(list 'primcall %eq? pa0 pa1)
     ;;(list (list 'cjump 'eq pa0 pa1 true-label false-label))
     (if (eqv? false-label next-label)
         (list (list 'beq pa0  pa1 true-label))
         (list (list 'beq pa0  pa1 true-label)
               (list 'beq reg0 reg0   false-label)))]
    [(list 'primcall %zero? pa0)
     (if (eqv? false-label next-label)
         (list (list 'beq reg0 pa0 true-label))
         (list (list 'beq reg0 pa0 true-label)
               (list 'beq reg0 reg0   false-label)))]
    [(list 'not cond)
     (gen-pred-code cond false-label true-label next-label)]
    [(list-rest 'and conditions)
     (gen-sequence conditions
                   (lambda (following)
                     (values (or following true-label)
                             false-label)))]
    [(list-rest 'or conditions)
     (gen-sequence conditions
                   (lambda (following)
                     (values true-label
                             (or following false-label))))]))

;; simple stack frame handling for now
(define *frame-size* 16)

(define *debug-codegen* #f)

(define (gen-ir code-exp entry-label (fun-name "<anon>"))
  (match-define (list 'code (list formals ...) body-exp)
                code-exp)
  (let* ([env (make-env (global-env))]
         [insns empty]
         [n-temp 0])
    (define (emit! . args)
      (if (list? (car args))
          (apply emit! (car args))
          (set! insns (cons args insns))))
    (define (emit-all! . l)
      (for-each (lambda (argl) (apply emit! argl)) l))
    (define (alloc-temp)
      (begin0
          (list 'temp n-temp)
        (set! n-temp (add1 n-temp))))
    (define (no-code? exp)
      (and (symbol? exp)
           (tagged-list? 'local (env-lookup env exp))))
    (define (cg exp dd cd next-label)
      (define (gen-tail)
        ;; unused
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
         [else (emit! 'beq 0 0 cd)]))
      (define (gen-children cl cr body-label)
        ;; XXX: need to improve this
        (cond
         [(or (no-code? cl) (no-code? cr))
          (list (cg cl #f body-label body-label)
                (cg cr #f body-label body-label))]
         [else
          (let* ([l-label (internal-label)]
                 [r-reg (cg cr #f l-label l-label)])
            (emit! 'label l-label)
            (list (cg cl #f body-label body-label)
                  r-reg))]))
      (define (gen-code)
        (match exp
          ;; constant reference
          [(? const?)
           (let* ([imm (immediate-rep exp)]
                  [cname (const-ref imm)]
                  [reg (or dd (alloc-temp))])
             (emit! 'lw 0 reg cname
                    (format "; ~a = ~a" reg exp))
             reg)]
          ;; register reference
          [(list 'register n)
           exp]
          ;; symbol
          [(? symbol?)
           (let* ([referent (env-lookup env exp)])
             (match referent
               ;; constant, from the constant pool
               [(list 'immediate val)
                (let* ([cname (const-ref val)]
                       [reg (or dd (alloc-temp))])
                  (emit! 'lw 0 reg cname
                         (format "; ~a = ~a" reg exp))
                  reg)]
               ;; register variable (formal param)
               [(list 'local name)
                referent]))]
          ;; unary primitive
          [(list 'primcall prim arg)
           (let* ([op-label (internal-label)]
                  [dest (or dd (alloc-temp))]
                  [arg-reg (cg arg #f op-label op-label)])
             (emit! 'label op-label)
             (match prim
               ['%bnot (emit! 'nand arg-reg arg-reg dest)])
             dest)]
          ;; binary primitive
          [(list 'primcall prim arg1 arg2)
           (let* ([op-label (internal-label)]
                  [child-regs (gen-children arg1 arg2 op-label)]
                  [t1 (car child-regs)]
                  [t2 (cadr child-regs)]
                  [dest (or dd (alloc-temp))])
             (emit! 'label op-label)
             (match prim
               ['%add  (emit! 'add t1 t2 dest)]
               ['%nand (emit! 'nand t1 t2 dest)]
               ['%band (emit! 'nand t1 t2 dest)
                       (emit! 'nand dest dest dest)])
             dest)]
          ;; function call
          [(list 'labelcall sym args ...)
           ;; marshal arguments
           (let* ([next-label (internal-label)]
                  [arg-temps
                   (for/list ([arg args])
                     (if (no-code? arg)
                         (cg arg #f next-label next-label)
                         (begin0
                             (cg arg #f next-label next-label)
                           (emit! 'label next-label)
                           (set! next-label (internal-label)))))]
                  [dest-reg (or dd (alloc-temp))]
                  [target (lambda-addr-label (cadr (env-lookup env sym)))])
             ;; TODO: check for tail call
             (apply emit! 'labelcall dest-reg target arg-temps)
             dest-reg)]
          ;; conditional
          [(list 'if if-pred if-then if-else)
           (let* ([true-label (internal-label)]
                  [false-label (internal-label)]
                  [branch-label (internal-label)]
                  [dest (or dd (alloc-temp))])
             (for ([stmt (gen-pred-code if-pred
                                        true-label false-label false-label)])
               (match stmt
                 [(list 'label (? string?))
                  (apply emit! stmt)]
                 [(list 'beq arg0 arg1 target-label)
                  (let* ([branch-label (internal-label)]
                         [registers (gen-children arg0 arg1 branch-label)])
                    (emit! 'label branch-label)
                    (emit! 'beq (car registers) (cadr registers) target-label))]))
             (emit! 'label false-label)
             (cg if-else dest cd true-label)
             (emit! 'label true-label)
             (cg if-then dest cd next-label))]))      
      (let ([result (gen-code)])
        (cond
         [(tagged-list? 'if exp) #f]
         ;; expression in tail position, return
         [(eq? cd 'return)
          (emit! 'return result 'return-addr)]
         ;; continue directly to next label (or whatever)
         ;; when next-label is #f or the same as cd
         [(or (not next-label) (eq? cd next-label)) #t]
         ;; jump to next label
         [else (emit! 'beq 0 0 cd)])
        result))
    
    (when *debug-codegen*
      (trace alloc-temp)
      (trace cg))
    (emit! 'noop (format "; [~a] ~v" fun-name code-exp))
    ;; function entry point
    (emit! 'label entry-label)
    (emit! 'prologue)
    (emit! 'bind 'return-addr 6 '(frame 0))
    (for ([formal formals]
          [reg (in-range 1 4)])
      (let ([var-ref (list 'local formal)])
        (env-define env formal var-ref)
        (emit! 'bind var-ref reg (list 'frame reg))))
    (let ([temp (alloc-temp)])
      (cg body-exp temp 'return #f))
    (reverse insns)))

(define (ir-dest stmt)
  (let* ([tag (car stmt)]
         [dest
          (case tag
            [(add nand) (fourth stmt)]
            [(lw) (third stmt)]
            [(labelcall) (second stmt)]
            [(bind) (second stmt)]
            [(sw beq noop label prologue return) #f]
            [else (error "unhandled tag: " tag)])])
    (if (or (eq? dest 0) (equal? dest '(register 0)))
        #f
        dest)))

(define (ir-sources stmt)
  (let ([tag (car stmt)])
    (remove* '(0 (register 0))
             (case tag
               [(add nand) (list (second stmt) (third stmt))]
               [(lw) (list (second stmt))]
               [(sw) (list (second stmt) (third stmt))]
               [(labelcall) (cdddr stmt)]
               [(beq) (list (second stmt) (third stmt))]
               [(return) (list (second stmt) (third stmt))]
               [(noop label bind prologue) empty]
               [else (error "unhandled tag: " tag)]))))

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

(define (location-stack-loc l)
  (car l))

(define (location-spill-pos l)
  (cadr l))

;; Register allocator
;;
;; This implements a simple linear scan register allocator as
;; described in Poletto and Sarkar, 1999, and with some of the
;; adjustments discussed in Sagonas and Stenman, 2003.

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

(define (linear-scan-alloc code)
  (let* ([intervals (linear-scan-build-intervals code)]
         [active empty]
         [free '(1 2 3 6)]
         [r (length free)]
         [next-spill-loc 0]
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
                              (assign-location (interval-var i)
                                               (interval-start i))))))))
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
    (values assignments
            (map (lambda (int live)
                   (append int (list live)))
                 intervals
                 (reverse live-at))
            (dict-set* empty
                       'fixed-loc fixed-loc
                       'num-spill-locs next-spill-loc))))

(define (last-interval-for i intervals)
  (let ([succ (and (pair? (cdr intervals))
                   (cadr intervals))])
    (if (or (not succ) (< i (interval-start succ)))
        (car intervals)
        (last-interval-for i (cdr intervals)))))

(define no-label "      ")

(define spill-dest-reg 5)
(define spill-s1-reg 4)
(define spill-s2-reg 5)

(define (spill-sp-offset loc)
  (+ 5 loc))

;; analyze-frame: wraps the register allocator, performs extra
;; post-processing and analysis.
;;
;; (analyze-frame code sasm)
;;   code: function definition, as:
;;       (code (arg1 arg2 ...) body-expr)
;;   sasm: low-level 'symbolic assembly' IR from gen-ir
;;
;; Returns values:
;;   assignments: alist dict
;;     keys:   (temp N) | return-addr
;;     values: (register N) | (spill N)
;;
;;   intervals: list of interval record lists, each as follows:
;;     (var interval-begin interval-end asm)
;;     var: a valid key for assignments
;;     interval-begin, interval-end: positions in sasm
;;     asm: record from sasm
;;
;;   frame-info: alist dict

(define (analyze-frame code sasm)
  (let*-values ([(assignments intervals frame-info)
                 (linear-scan-alloc sasm)]
                [(is-leaf) (not (findf (curry tagged-list? 'labelcall)
                                       sasm))]
                [(num-formals) (length (code-formals code))]
                [(stack-formals) (max 0 (- num-formals 3))]
                [(regs-used) (apply set (filter (curry tagged-list? 'register)
                                                (dict-values assignments)))]
                [(num-reg) (set-count regs-used)]
                [(spill-locs) (dict-ref frame-info 'num-spill-locs)]
                [(spill-size) (+ spill-locs num-reg)]
                [(fixed-loc) (dict-ref frame-info 'fixed-loc)]
                [(reg-save) (for/list ([reg-entry regs-used]
                                       [loc (in-naturals spill-locs)])
                              (match reg-entry
                                [(list 'register reg)
                                 (cons reg
                                       (list 'spill loc))]))]
                [(stack-assignments)
                 (for/list ([(var loc) (in-dict assignments)])
                   (let ([fixed (dict-ref fixed-loc var #f)])
                     (if fixed
                         (cons var fixed)
                         (match loc
                           [(list (or 'spill 'frame) n)
                            (cons var loc)]
                           [(list 'register n)
                            (cons var (dict-ref reg-save n))]))))]
                [(frame-size) (+ 1 stack-formals spill-size)]
                [(skip-frame-setup) (and is-leaf
                                         (= num-reg (dict-count assignments)))])
    (values assignments
            intervals
            (dict-set* frame-info
                       'is-leaf is-leaf
                       'spill-offset spill-size
                       'stack-assignments stack-assignments
                       'frame-size frame-size
                       'skip-frame-setup skip-frame-setup))))

(define/match (register-num r)
  [((list 'register n)) n])

;; Stack frame layout:
;;
;; The first three arguments are passed in r1-r3. Any others should be
;; passed on the stack.
;;
;; Offsets given are from the caller's stack pointer, which is
;; callee-save. A leaf function could use this as a frame pointer; a
;; non-leaf function should decrement it to be its own stack pointer.

;; 0: return address, 1 word
;; arguments 4+, (max 0 (- (length args) 3)) words
;; spill area, (length locations) words

;; 0: <callee frame start>
;; 1+: arguments 

(define (internal-label? name)
  (and (string? name)
       (eqv? (string-ref name 0) #\I)))

(define (label-cleanup sasm)
  (let*-values ([(_ canonical used)
                 (for/fold ([cur-label #f]
                            ;; label => canonical label name
                            [int-labels empty]
                            [labels-used (set)])
                     ([stmt sasm])
                   (match stmt
                     ;; internal label
                     [(list 'label (? internal-label? name))
                      (if cur-label
                          ;; later label, to alias
                          (values cur-label
                                  (dict-set int-labels name cur-label)
                                  labels-used)
                          ;; first label in a group
                          (values name
                                  (dict-set int-labels name name)
                                  labels-used))]
                     ;; branch to internal label
                     [(list 'beq a b (? internal-label? target))
                      (values #f int-labels (set-add labels-used target))]
                     ;; non-code-generating directive
                     [(list 'bind args ...)
                      (values cur-label int-labels labels-used)]
                     ;; anything else
                     [else
                      (values #f int-labels labels-used)]))]
                [(to-emit)
                 (list->set (map (curry dict-ref canonical) (set->list used)))])
    (filter (lambda (stmt)
              (match stmt
                [(list 'label name)
                 (or (not (internal-label? name))
                     (set-member? to-emit name))]
                [else
                 #t]))
            sasm)))

;; gen-asm: Generates actual LC2K assembly from the low-level IR.
;;
;; - Calls linear-scan-alloc to perform register allocation.
;; - Inserts loads and stores as appropriate for stack-resident vars.
;; - Generates function prologue, call and return sequences from
;;   prologue, labelcall, and return directives.
;;
(define (gen-asm code sasm)
  (let*-values
      ([(assignments intervals frame-info) (analyze-frame code sasm)]
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
      [((list 'frame n)) (- (dict-ref frame-info 'frame-size) n)]
      [((list 'spill n)) (- (dict-ref frame-info 'spill-offset) n)])
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
                 [reg (and (tagged-list? 'register loc)
                           (register-num loc))]
                 [remain (cdr ctx)]
                 [conflict (and reg ;; var in our destination register 
                                (findf (lambda (a)
                                         (equal? (dict-ref assignments a)
                                                 (list 'register dest)))
                                       remain))]
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
      (for-each emit-line before)
      (apply emit-unwrapped! dir)
      (for-each emit-line after)
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
        [(list 'labelcall dest-var label args ...)
         (let ([target-reg spill-s2-reg])
           (save-regs-for-call pos dest-var)
           ;; marshal first 3 args into registers
           (marshal-args args pos)
           (emit! 'lw 0 target-reg label
                  "; load target address")
           (emit! 'jalr target-reg ret-reg (format "; call ~a" label))
           (match (dict-ref assignments dest-var)
             [(list 'register 1) #t]
             [(list 'register n) (emit! 'add 0 1 n "; store result")]
             [(? list? stack-loc) (emit-line (store-spilled 1 stack-loc))])
           ;; otherwise we wanted the value in r1, and it's there now
           ;; restore live vars
           (restore-regs-for-call pos dest-var))]
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

(define *code-source* (make-parameter #f))
(define *code-elt* (make-parameter #f))

(struct pass (name proc arg-keys))

(define (pass-args pass env)
  (map (curry dict-ref env)
       (pass-arg-keys pass)))

(define (run-pass pass env)
  (dict-set env (pass-name pass)
            (apply (pass-proc pass)
                   (pass-args pass env))))

(define compiler-passes
  (list (pass 'desugar
              expand-prims '(code))
        (pass 'ir1
              gen-ir '(desugar label name))
        (pass 'ir1a
              label-cleanup '(ir1))
        (pass 'asm
              gen-asm '(code ir1a))
        (pass 'output
              (curry for-each displayln) '(asm))))

(define (compile-fun code label name)
  (parameterize ([*code-elt* name])
    (for/fold ([comp-env (list (cons 'code code)
                               (cons 'label label)
                               (cons 'name name))])
        ([pass compiler-passes])
      (with-handlers
          ([exn:fail?
            (lambda (exn)
              (eprintf "Error compiling ~a (from ~a)!~n"
                       (*code-elt*) (*code-source*))
              (pretty-print code (current-error-port))
              (eprintf "Error in pass ~a (~v), with args:~n"
                       (pass-name pass)
                       (pass-proc pass))
              (for ([key (pass-arg-keys pass)]
                    [arg (pass-args pass comp-env)])
                (eprintf "~a: ~a~n"
                         key (pretty-format arg)))
              (raise exn))])
        (run-pass pass comp-env)))))

(define (compile-code prog origin (compile-toplevel #f))
  (parameterize ([*code-source* origin])
    (match prog
      [(list 'labels
             (list (list lvars lexprs) ...)
             top-expr)
       (for ([lvar lvars]
             [lexpr lexprs])
         (let ([label (lambda-label)])
           (env-define (global-env) lvar (list 'fun-label label))
           (compile-fun lexpr label lvar)))
       (when compile-toplevel
         (let ([top-label (lambda-label)])
           (compile-fun `(code () ,top-expr) top-label "<toplevel>")         
           top-label))])))

(define *scheme-runtime-file* "runtime.scm")

(define (load-code file)
  (transform-program (file->list file)))

(define (compile-top prog)
  (parameterize ([current-trace-notify
                  (lambda (t)
                    (displayln t (current-error-port)))])
    (init-global-env)
    ;; write the raw asm runtime directly
    (for-each displayln *runtime-text*)
    ;; compile the Scheme runtime
    (compile-code (load-code *scheme-runtime-file*)
                  *scheme-runtime-file*)
    ;; compile the user code
    (let ([entry-label (compile-code prog 'user #t)])
      ;; and now write the runtime support data
      (for-each displayln (runtime-data entry-label)))
    ;; and constant pool
    (write-constant-defs)
    ;; and now the function entry points
    (for ([label-entry (in-hash-values (env-dict (global-env)))]
          #:when (tagged-list? 'fun-label label-entry))
      (let ([label-name (cadr label-entry)])
        (displayln (format "~a    .fill ~a"
                           (lambda-addr-label label-name)
                           label-name))))))

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
