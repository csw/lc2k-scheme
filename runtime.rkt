#lang racket

;;;; runtime: support for the LC2K assembly and Scheme code making up
;;;; the runtime environment and standard library.

(require racket/runtime-path)

(require "types.rkt")
(require "environ.rkt")

(provide runtime-preamble runtime-data *lc2k-rv* *scheme-runtime-file*
         asm-procs)

(define *lc2k-rv* "SCMrv")

;; A very basic runtime system. Sets up the stack pointer in register
;; 7, pointing to the end of memory, calls the procedure at the
;; 'entry' label, stores the return value found in register 1 to the
;; appropriate location, and halts.
;;
;; The heap starts at 8192 and grows up; the stack grows down from
;; 65535.

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

;; The bulk of the runtime code is Scheme code, here.

(define-runtime-path *scheme-runtime-file* "rt-code.scm")

;; The basic list procedures are implemented in assembly, here.
;; Others could be as well.

(define asm-procs
  (list (proc 'cons -1 "Lcons" "Acons" "Pcons" #f)
        (proc 'car  -1 "Lcar"  "Acar"  "Pcar"  #f)
        (proc 'cdr  -1 "Lcdr"  "Acdr"  "Pcdr"  #f)))

(define asm-proc-asm
  (list (cons 'cons
              #("        noop"
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
              #("        noop"
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

(for ([aproc asm-procs])
  (set-proc-asm! aproc (dict-ref asm-proc-asm (proc-name aproc))))

