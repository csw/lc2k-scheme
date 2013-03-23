#lang racket

(require "types.rkt")

(provide global-env make-env write-constant-defs clear-env
         env-define env-lookup const-label raw-const-label
         internal-label
         (struct-out proc) make-proc proc-pointer)

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

(define (const-label val)
  (raw-const-label (immediate-rep val)))

(define (raw-const-label raw)
  (if (hash-has-key? constants raw)
      (hash-ref constants raw)
      (let ([cname (format "C~a" constant-n)])
        (hash-set! constants raw cname)
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


(define (clear-env)
  (set! num-environments 0)
  (set! environments empty)
  (make-env #f)
  (set! constants (make-hash))
  (set! constant-n 0)
  (set! lambda-n 0)
  (set! internal-n 0))

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



