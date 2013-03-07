#lang racket

(provide run-lc2k compile-and-run compile-ret)

(require "compiler.rkt")

(define *lc2k-path* (expand-user-path "~/Dropbox/school/370/code/p1"))
;(define *assembler* (build-path *lc2k-path* "assemble"))
;(define *simulator* (build-path *lc2k-path* "simulator"))
(define *runner* (build-path *lc2k-path* "runner"))

(define (run-lc2k prog result-loc)
  (let ([out (with-output-to-string
               (lambda ()
                 (system* *runner* "-p" prog "-m" result-loc)))])
    (with-input-from-string out read-line)))

(define (compile-and-run x)
  (let ([tmpf (make-temporary-file "lc2kscm-~a.as")])
    (compile-to x tmpf)
    (run-lc2k tmpf *lc2k-rv*)))

(define (compile-ret x)
  (decode-immediate (string->number (compile-and-run x))))
