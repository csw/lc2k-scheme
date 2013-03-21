#lang racket

(provide run-lc2k compile-and-run compile-ret)

(require racket/runtime-path)
(require "compiler.rkt")

(define *lc2k-path* (or (getenv "LC2K")
                        (expand-user-path "~/Dropbox/school/370/code/p1")))
(define *assembler* (build-path *lc2k-path* (or (getenv "ASM")
                                                "assemble")))
(define *simulator* (build-path *lc2k-path* (or (getenv "SIM")
                                                "simulator")))
(define-runtime-path *runner* "runner")

(define (run-lc2k prog result-loc)
  (let ([out (with-output-to-string
               (lambda ()
                 (system* *runner*
                          "-a" *assembler* "-s" *simulator*
                          "-p" prog "-m" result-loc)))])
    (with-input-from-string out read-line)))

(define (compile-and-run x)
  (let ([tmpf (make-temporary-file "lc2kscm-~a.as")])
    (compile-to x tmpf)
    (run-lc2k tmpf *lc2k-rv*)))

(define (compile-ret x)
  (decode-immediate (string->number (compile-and-run x))))

(define (run-file path)
  (compile-ret (file->list path)))

(module+ main
  (displayln (run-file (vector-ref (current-command-line-arguments) 0))))
