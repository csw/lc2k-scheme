#lang racket

(require "driver.rkt")
(require "compiler.rkt")

(define (run-tests)
  (let ([section #f])
    (define (test-section s)
      (set! section s))
    (define (test-case code expect)
      (displayln (format "Testing ~a" code))
      (let* ([wrapped `(lambda () ,code)]
             [result (compile-ret wrapped)])
        (unless (equal? result expect)
          (raise-arguments-error 'test-case
                                 "Obtained wrong result"
                                 "section" section
                                 "code"   code
                                 "expect" expect
                                 "result" result))))
    (test-section "Constant loading and returning")
    (test-case '58 58)
    (test-case '0 0)
    (test-section "Non-numeric constants")
    (test-case #t #t)
    (test-case #f #f)
    (test-case #\a #\a)
    (test-case #\Z #\Z)
    (test-case empty empty)
    ;;(test-section "Binary primitives")
    ;;(test-case '(+ 3 5) 8)
    (displayln "All tests succeeded.")))
