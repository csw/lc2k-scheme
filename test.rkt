#lang racket

(require "driver.rkt")
(require "compiler.rkt")

(define (run-tests)
  (let ([section #f])
    (define (test-section s)
      (set! section s))
    (define (test-case code (expect #f))
      (displayln (format "Testing ~a" code))
      (let* ([expect-r (or expect (eval code))]
             [result (compile-ret code)])
        (unless (equal? result expect-r)
          (raise-arguments-error 'test-case
                                 "Obtained wrong result"
                                 "section" section
                                 "code"   code
                                 "expect" expect-r
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
    (test-section "Unary primitives")
    ;;(test-case '(zero? 0))
    ;;(test-case '(zero? 9))
    (test-section "Binary primitives")
    (test-case '(+ 3 5) 8)
    (test-case '(+ (+ 3 5) 4) 12)
    (test-section "Lambdas")
    ;;(test-case '((lambda (n) (+ 5 n)) 9) (+ 5 9))
    (test-section "Simple conditionals")
    (test-case '(if (empty? empty) 1 99))
    (test-case '(+ (if (empty? empty) 1 99)
                   (+ (if (zero? 1) 2000 20)
                      373)))
    (test-section "Conses")
    ;;(test-case '(empty? empty) #t)
    (displayln "All tests succeeded.")))
