#lang racket

;; compiler.rkt
;; Scheme compiler for the LC2K
;;
;; Clayton Wheeler <cswh@umich.edu>, 2013

(require racket/dict)
(require racket/format)
(require racket/trace)
(require racket/pretty)

(require "util.rkt")
(require "types.rkt")
(require "frontend.rkt")
(require "environ.rkt")
(require "ir.rkt")
(require "linear-scan.rkt")
(require "lc2k.rkt")
(require "backend.rkt")
(require "runtime.rkt")

(provide compile-program compile-to *lc2k-rv* decode-immediate
         compile-print-file)

;; This is the top-level module of the compiler. The interesting code
;; is elsewhere:
;;
;; types: defines how tagged immediates and pointers work.
;; environ: manages namespaces, constant pools, procedures.
;; runtime: basic runtime support for executing Scheme code.
;; rt-code.scm: Scheme runtime code.
;; lc2k: a few basic machine definitions.
;; util: a few generally-useful functions.
;;
;; frontend: preliminary transformations on Scheme code.
;; ir: generates pseudo-assembler IR with virtual registers.
;; linear-scan: allocates virtual registers to real ones or the stack.
;; backend: generates LC2K assembly code.
;;
;; driver: wrapper to compile, assemble, run, and display output.
;; runner: Ruby script to drive the assembler and simulator, parse output.
;; test: test suite, showing the currently supported features.

;; immed-constants:
;;
;; Built-in immediate (i.e. not pointer types) constants available to
;; Scheme code. The car is the symbol they will be bound under.
;;
;; Note that the % prefix, in general, marks implementation-level code
;; or data not intended for use in user code.

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
  ;; reset everything
  (clear-env)
  ;; define basic constants
  (for ([def immed-constants])
    (env-define (global-env)
                (car def)
                (list 'immediate (cdr def))))
  ;; define asm procedures
  (for ([aproc asm-procs])
    (env-define (global-env) (proc-name aproc) aproc)))

(init-global-env)

;; Parse a list of symbols from an environment variable.

(define (env-symbols var)
  (let ([value (getenv var)])
    (if value
        (map string->symbol (string-split value))
        empty)))

;; For compilation debugging:
;;
;; Set the 'trace' environment variable to a space-separated list of
;; function names to trace the compilation of. To trace compilation of
;; a top-level expression, use the dummy name %toplevel.
;;
;; For example:
;;
;; $ trace="sum %toplevel" racket -t compiler.rkt test/sum-1.scm

(define *trace-compilation-of* (env-symbols "trace"))
(define *trace-compilation* (make-parameter #f))

(struct pass (name proc arg-keys))

(define (pass-args pass env)
  (map (curry dict-ref env)
       (pass-arg-keys pass)))

(define (run-pass pass env)
  (let ([result (apply (pass-proc pass)
                       (pass-args pass env))])
    (when (*trace-compilation*)
      (eprintf "~a:~n~a~n~n"
               (pass-name pass) (pretty-format result)))
    (dict-set env (pass-name pass) result)))

;; Define new compiler passes here.

(define compiler-passes
  (list (pass 'desugar
              expand-prims '(code))
        (pass 'ir1
              gen-ir '(desugar label name))
        (pass 'ir1a
              label-cleanup '(ir1))
        (pass 'register-live-intervals
              linear-scan-build-intervals '(ir1a))
        (pass 'register-alloc
              linear-scan-alloc '(ir1a register-live-intervals))
        (pass 'frame-info
              analyze-frame '(code ir1a register-alloc))
        (pass 'asm
              gen-asm '(code ir1a register-alloc frame-info))
        (pass 'asm-vec
              list->vector '(asm))
        (pass 'store
              set-proc-asm! '(proc asm-vec))))

;; Track what's being compiled, to enable useful error reporting.

(define *code-source* (make-parameter #f))
(define *code-elt* (make-parameter #f))

(define (compile-fun cproc)
  (parameterize ([*code-elt* (proc-name cproc)]
                 [*trace-compilation* (memq (proc-name cproc)
                                            *trace-compilation-of*)])
    (when (*trace-compilation*)
      (eprintf "==== Compiling ~a ====~n~n" (proc-name cproc))
      (eprintf "code:~n~a~n~n" (pretty-format (proc-code cproc))))
    (for/fold ([comp-env (list (cons 'code (proc-code cproc))
                               (cons 'label (proc-label cproc))
                               (cons 'name (proc-name cproc))
                               (cons 'proc cproc))])
        ([pass compiler-passes])
      (with-handlers
          ([exn:fail?
            (lambda (exn)
              (eprintf "Error compiling ~a (from ~a)!~n"
                       (*code-elt*) (*code-source*))
              (pretty-print (proc-code cproc) (current-error-port))
              (eprintf "Error in pass ~a (~v), with args:~n"
                       (pass-name pass)
                       (pass-proc pass))
              (for ([key (pass-arg-keys pass)]
                    [arg (pass-args pass comp-env)])
                (eprintf "~a: ~a~n"
                         key (pretty-format arg)))
              (raise exn))])
        (run-pass pass comp-env)))
    (when (*trace-compilation*)
      #f)))

(define toplevel-proc-name '%toplevel)

(define (compile-code prog origin (compile-toplevel #f))
  (parameterize ([*code-source* origin])
    (match prog
      [(list 'labels
             (list (list lvars lexprs) ...)
             top-expr)
       (let ([procs
              (for/list ([lvar lvars]
                    [lexpr lexprs])
                (let ([lproc (make-proc lvar lexpr)])
                  (env-define (global-env) lvar lproc)
                  (compile-fun lproc)
                  lproc))])
         (if compile-toplevel
             (let* ([code `(code () ,top-expr)]
                    [tproc (make-proc toplevel-proc-name code)])
               (compile-fun tproc)
               (append procs (list tproc)))
             procs))])))

(define (load-code file)
  (transform-program (file->list file)))

(define (compile-top prog)
  (parameterize ([current-trace-notify
                  (lambda (t)
                    (displayln t (current-error-port)))])
    (init-global-env)
    ;; compile all the code
    (let* ([runtime-procs
            (compile-code (load-code *scheme-runtime-file*)
                          *scheme-runtime-file*)]
           [user-procs (compile-code prog 'user #t)]
           [entry-proc (findf (lambda (proc)
                                (eq? (proc-name proc) toplevel-proc-name))
                              user-procs)]
           [all-procs (append asm-procs
                              runtime-procs
                              user-procs)])
      ;; write the raw asm runtime directly
      (for-each displayln runtime-preamble)
      ;; write the procedure bodies
      (for/fold ([address (length runtime-preamble)])
          ([proc all-procs])
        ;; track the address of each proc
        ;; (allowing for the leading noop)
        (set-proc-address! proc (add1 address))
        ;; and write it out
        (for ([line (proc-asm proc)])
          (displayln line))
        (+ address (vector-length (proc-asm proc))))
      ;; and now write the runtime support data
      (for-each displayln (runtime-data (proc-label entry-proc)))
      ;; and constant pool
      (write-constant-defs)
      ;; and now the function entry points
      (for ([proc all-procs])
        ;; raw address
        (displayln (format "~a    .fill ~a"
                           (proc-addr-label proc)
                           (proc-label proc)))
        ;; tagged pointer
        (displayln (format "~a    .fill ~a"
                           (proc-ptr-label proc)
                           (proc-pointer proc)))))))

(define (compile-program prog)
  (compile-top (transform-program prog)))

(define (compile-to x path)
  (with-output-to-file path
    (lambda ()
      (compile-program x))
    #:exists 'truncate))

(define (compile-file-to prog-file out-path)
  (with-output-to-file out-path
    (lambda ()
      (compile-program (file->list prog-file)))
    #:exists 'truncate))

(define (compile-print-file path)
  (compile-program (file->list path)))

(module+ main
  (compile-print-file (vector-ref (current-command-line-arguments) 0)))
