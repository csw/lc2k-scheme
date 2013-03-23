#lang racket

(require racket/trace)

(require "types.rkt")
(require "environ.rkt")
(require "lc2k.rkt")
(require "util.rkt")

(provide gen-ir ir-dest ir-sources register-num label-cleanup)

(define (reg-ref n)
  (list 'register n))

(define reg0 (reg-ref 0))

(define/match (register-num r)
  [((list 'register n)) n])

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
         [post-prologue (internal-label)]
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
    ;;
    ;; cost-estimate: rough relative measure of the local cost of
    ;;     function args, in order to evaluate the most expensive ones
    ;;     first to minimize temp lifetimes.
    ;;
    (define/match (cost-estimate exp)
      [((? immed-const?)) 1]
      [((? symbol?))
       (match (env-lookup env exp)
         ;; needs to match all environment entry types
         ;; as defined in environ.rkt
         [(list 'local name) 0]
         [(list 'immediate val) 1]
         [(struct proc _) 1])]
      [((list 'primcall _ ...)) 2]
      [((list 'call _ ...)) 6]
      [((list 'if _ ...)) 3])
    (define (no-code? exp)
      (and (symbol? exp)
           (tagged-list? 'local (env-lookup env exp))))
    (define (cg exp dd cd next-label)
      (define (gen-args args)
        (let ([arg-temp-alist
               (for/list ([arg (sort args >
                                     #:key cost-estimate
                                     #:cache-keys? #t)])
                 (cons arg (cg arg #f #f #f)))])
          (map (compose cdr (curryr assq arg-temp-alist))
               args)))
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
          ;; immediate constant reference
          [(? immed-const?)
           (if (and dd (not (eq? cd 'return)))
               (begin
                 (emit! 'lw 0 dd (const-label exp)
                        (format "; ~a = ~a" dd exp))
                 dd)
               (list 'constant exp exp))]
          ;; register reference
          [(list 'register n)
           exp]
          ;; symbol
          [(? symbol?)
           (let* ([referent (env-lookup env exp)])
             (match referent
               ;; constant, from the constant pool
               [(list 'immediate val)
                (if (and dd (not (eq? cd 'return)))
                    (begin
                      (emit! 'lw 0 dd (const-label val)
                             (format "; ~a = ~a" dd exp))
                      dd)
                    (list 'constant val exp))]
               ;; register variable (formal param)
               [(list 'local name)
                referent]
               ;; procedure; take a pointer to it
               [(struct proc _)
                (let* ([reg (or dd (alloc-temp))])
                  (emit! 'lw 0 reg (proc-ptr-label referent)
                         (format "; ~a = &~a" reg (proc-name referent)))
                  reg)]))]
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
               ['%add  (emit! 'add t1 t2 dest
                              (format "~a = ~a + ~a" dest t1 t2))]
               ['%nand (emit! 'nand t1 t2 dest
                              (format "~a = ~a NAND ~a" dest t1 t2))]
               ['%band (emit! 'nand t1 t2 dest)
                       (emit! 'nand dest dest dest
                              (format "~a = ~a & ~a" dest t1 t2))])
             dest)]
          ;; function call
          [(list 'call (? symbol? sym) args ...)
           ;; marshal arguments
           (let* ([next-label (internal-label)]
                  [arg-temps (gen-args args)]
                  [dest-reg (or dd (alloc-temp))]
                  [target-entry (env-lookup env sym)]
                  [target-label (and (proc? target-entry)
                                     (proc-label target-entry))])
             (if (eq? cd 'return)
                 (begin
                   (apply emit!
                          'tail-call
                          (if (and target-label
                                   (equal? entry-label target-label))
                              ;; self tail call, skip epilogue and prologue
                              post-prologue
                              ;; tail call elsewhere
                              target-entry)
                          'return-addr arg-temps)
                   'tail-call)
                 (begin
                   (match target-entry
                     [(struct proc _)
                      (apply emit! 'labelcall dest-reg target-entry arg-temps)]
                     [(list 'local var-name)
                      (apply emit! 'proc-call dest-reg target-entry arg-temps)])
                   dest-reg)))]
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
         ;; generated a tail call
         [(eq? result 'tail-call) #f]
         ;; otherwise shouldn't generate a control transfer
         [(not cd) #f]
         ;; expression in tail position, return
         [(eq? cd 'return)
          (emit! 'return result 'return-addr)]
         ;; continue directly to next label (or whatever)
         ;; when next-label is #f or the same as cd
         [(or (not next-label) (eq? cd next-label)) #t]
         ;; jump to next label
         [else (emit! 'beq 0 0 cd)])
        result))

    (when (tracing? 'cg)
      (trace cg))

    (emit! 'noop (format "; [~a] ~v" fun-name code-exp))
    ;; function entry point
    (emit! 'label entry-label)
    (emit! 'prologue)
    ;; branch target for self tail calls
    (emit! 'label post-prologue)
    ;; tell the register allocator where return-addr is
    (emit! 'bind 'return-addr 6 '(frame 0))
    (for ([formal formals]
          [reg (in-range 1 4)])
      (let ([var-ref (list 'local formal)])
        (env-define env formal var-ref)
        (emit! 'bind var-ref reg #f)))
    (let ([temp (alloc-temp)])
      (cg body-exp temp 'return #f))
    (reverse insns)))

(define (ir-dest stmt)
  (let* ([tag (car stmt)]
         [dest
          (case tag
            [(add nand) (fourth stmt)]
            [(lw) (third stmt)]
            [(labelcall proc-call) (second stmt)]
            [(bind) (second stmt)]
            [(sw beq noop label prologue return tail-call) #f]
            [else (error "unhandled tag: " tag)])])
    (if (or (eq? dest 0) (equal? dest '(register 0)))
        #f
        dest)))

(define (ir-sources stmt)
  (let ([tag (car stmt)])
    (filter-not (lambda (e)
                  (or (eq? 0 e)
                      (tagged-list? 'constant e)
                      (equal? '(register 0) e)))
             (case tag
               [(add nand) (list (second stmt) (third stmt))]
               [(lw) (list (second stmt))]
               [(sw) (list (second stmt) (third stmt))]
               [(labelcall) (cdddr stmt)]
               [(proc-call) (cddr stmt)]
               [(tail-call)
                (match stmt
                  [(list 'tail-call (and target (list 'local _)) _ ...)
                   (cdr stmt)]
                  [else
                   (cddr stmt)])]
               [(beq) (list (second stmt) (third stmt))]
               [(return) (list (second stmt) (third stmt))]
               [(noop label bind prologue) empty]
               [else (error "unhandled tag: " tag)]))))

(define (internal-label? name)
  (and (string? name)
       (eqv? (string-ref name 0) #\I)))

;; Remove internal labels which are not referenced, and combine
;; sequences of label declarations, rewriting references.

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
                     ;; self-tail-call
                     [(list 'tail-call (? string? target) _ ...)
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

