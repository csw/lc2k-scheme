#lang racket

(provide tagged-list? const? env-symbols
         *trace-compilation* *trace-flags* tracing?)

;; env-symbols: string -> list-of-symbols
;;
;; Parse a list of symbols from an environment variable.

(define (env-symbols var)
  (let ([value (getenv var)])
    (if value
        (map string->symbol (string-split value))
        empty)))


;; *trace-compilation*: true when compilation of the current function
;; should be traced.

(define *trace-compilation* (make-parameter #f))

(define *trace-flags* (env-symbols "traceflags"))

(define (tracing? (flag empty))
  (and (*trace-compilation*)
       (memq flag *trace-flags*)))

(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

(define (const? v)
  (or (integer? v)
      (char? v)
      (boolean? v)
      (empty? v)))

