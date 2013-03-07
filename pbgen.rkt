#lang racket

(define (sieve-eratosthenes n)
  (letrec ([candidates (make-vector n 1)]
           [clear-multiples
            (lambda (c v)
              (when (< v n)
                (vector-set! candidates v 0)
                (clear-multiples c (+ v c))))])
    (vector-set! candidates 0 0)
    (vector-set! candidates 1 0)
    (for ([i (in-range 2 n)])
      (let ([val (vector-ref candidates i)])
        (when (not (zero? val))
          (clear-multiples i (* i 2)))))
    (for/list ([i (in-range 0 n)]
               [val candidates]
               #:when (eqv? val 1))
      i)))

(define (prime-factors n)
  (let ([primes (sieve-eratosthenes (add1 (integer-sqrt n)))]
        [fac empty])
    (for ([i primes]
          #:when (zero? (modulo n i)))
      (set! fac (cons (list i (/ n i)) fac)))
    (if (empty? fac)
        (list n 1)
        fac)))

(define (prime-factors-3 n)
  (let loop ([n n] [m 2] [factors (list)])
    (cond [(= n 1) factors]
          [(= 0 (modulo n m)) (loop (/ n m) 2 (cons m factors))]
          [else (loop n (add1 m) factors)])))

(define (gen-apply-num n)
  (let ([factors (prime-factors-3 n)]
        [])))
