#lang racket

;;;; runtime: support for the LC2K assembly and Scheme code making up
;;;; the runtime environment and standard library.

(require racket/runtime-path)

(require "types.rkt")

(provide runtime-preamble runtime-data *lc2k-rv* *scheme-runtime-file*)

(define *lc2k-rv* "SCMrv")

(define-runtime-path *scheme-runtime-file* "rt-code.scm")

(define runtime-preamble
  '("        lw   0 5 entry"
    "        lw   0 7 stack"
    "        jalr 5 6"
    "        sw   0 1 SCMrv"
    "SCMh    halt"))

(define (runtime-data entry-pt-label)
  (list "stack   .fill 65535"
        "heapS   .fill 8192"
        "heap    .fill 8192"
        "SCMrv   .fill 559038737"
        "consS   .fill 2"
        (format "entry   .fill ~a" entry-pt-label)
        (format "ctag    .fill ~a" cons-tag)
        (format "pmask   .fill ~a" pointer-mask)))

