#lang scheme

(define n 14)
(define r 7)

(define fac (make-vector 32 0))

(define (apply-n14 nmr e2)
  (if (zero? nmr)
      e2
      (begin
        (vector-set! fac 7 (+ 1 (vector-ref fac 7)))
        (apply-n13 (- nmr 1) (+ 2 e2)))))

(define primes '(29 23 19 17 13 11 7 5 3 2))

(define mul-primes (acc primes)
  (if (null? primes)
      acc
      (let ((p (car primes)))
        (mul-primes (* acc (expt p (vector-ref fac p)))
                    (cdr primes)))))

(define (multiply-first primes)
  (let* ((prime (car primes))
         (exponent (vector-ref fac prime)))
    (if (zero? e)
        (multiply-first (cdr primes))
        (mul-primes (expt prime exponent)
                    (cdr primes)))))

(define (apply-numerator n nmr)
  )

(define (calc-binom n nmr)
  (cond
   (= 0 r) 1
   (= 1 r) n
   else
   (begin
     (apply-numerator n nmr)
     (apply-denominator r)
     (multiply-first n))))

(define (binom-top)
  (let ((nmr (- n r)))
    (if (< nmr r)
        (begin                          ; swap
          (set! r nmr)
          (calc-binom n r))
        (calc-binom n nmr))))


