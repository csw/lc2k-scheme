#lang racket

(require "types.rkt")

(provide init-global-env global-env make-env write-constant-defs
         env-define env-lookup const-ref
         internal-label
         (struct-out proc) make-proc proc-pointer
         asm-procs)


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

;; environment entry values:
;;
;; ('immediate val):   immediate constant
;; ('local     name):  local variable
;; <proc>: procedure 

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

(define (next-lambda)
  (begin0
      lambda-n
    (set! lambda-n (add1 lambda-n))))

(define (prefix-label p n)
  (format "~a~a" p n))

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


(struct proc (name num label addr-label ptr-label code
                   [asm #:mutable #:auto] [address #:mutable #:auto]))

(define (make-proc name expr)
  (let ([num (next-lambda)])
    (proc name
          num
          (prefix-label #\L num)
          (prefix-label #\A num)
          (prefix-label #\P num)
          expr)))

(define (proc-pointer proc)
  (bitwise-ior proc-tag constant-bit (proc-address proc)))

(define asm-procs
  (list (proc 'cons -1 "Lcons" "Acons" "Pcons" #f)
        (proc 'car  -1 "Lcar"  "Acar"  "Pcar"  #f)
        (proc 'cdr  -1 "Lcdr"  "Acdr"  "Pcdr"  #f)))

(define asm-proc-asm
  (list (cons 'cons
              #(    "        noop"
                    "Lcons   lw   0 5 heap"
                    "        lw   0 4 consS"
                    "        add  4 5 4"
                    "        sw   0 4 heap"
                    "        sw   4 1 0"
                    "        sw   4 2 1"
                    "        lw   0 5 ctag"
                    "        add  4 5 1"
                    "        jalr 6 5"))
        (cons 'car
              #(    "        noop"
                    "Lcar    lw   0 5 pmask"
                    "        nand 1 5 1"
                    "        nand 1 1 1"
                    "        lw   1 1 0"
                    "        jalr 6 5"
                    "        noop"))
        (cons 'cdr
              #("        noop"
                "Lcdr    lw   0 5 pmask"
                "        nand 1 5 1"
                "        nand 1 1 1"
                "        lw   1 1 1"
                "        jalr 6 5"))))

(define immed-constants
  `((empty           . ,empty-list-v)
    (#t              . ,true-v)
    (#f              . ,false-v)
    (%tagged-mask    . ,tagged-mask)
    (%tagged-tag     . ,tagged-tag)
    (%type-tag-mask  . ,type-tag-mask)
    (%char-tag       . ,char-tag)
    (%bool-tag       . ,bool-tag)
    (%cons-tag       . ,cons-tag)
    (%proc-tag       . ,proc-tag)
    (%sign-bit       . ,(expt 2 31))
    (%max-fixnum-bit . ,(expt 2 29))
    (%all-ones       . ,(bitwise-not 0))))

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
  (for ([aproc asm-procs])
    (env-define (global-env) (proc-name aproc) aproc)
    (set-proc-asm! aproc (dict-ref asm-proc-asm (proc-name aproc)))))
(init-global-env)
