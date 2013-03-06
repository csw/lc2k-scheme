#lang racket

(require racket/format)

(provide compile-program compile-to *lc2k-rv*)

(define *runtime-text*
  '("        lw   0 6 SCMe"
    "        jalr 6 5"
    "        sw   0 1 SCMrv"
    "SCMh    halt"))

(define *runtime-data*
  (list (format "SCMe    .fill ~s" (length *runtime-text*))
        "SCMrv   .fill 559038737"))

(define *lc2k-rv* "SCMrv")

(define (compile-program x)
  (let ([text (reverse *runtime-text*)]
        [data (reverse *runtime-data*)])
    (define (emit s . args)
      (set! text (cons (apply format s args) text)))
    (define (emit-data s . args)
      (set! data (cons (apply format s args) data)))
    ;; do stuff here
    (emit "    lw   0 1 Cn")
    (emit "    jalr 5 6")
    (emit-data "Cn    .fill ~s" x)
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
