(define (fib-aux n a b)
  (if (zero? n)
      a
      (fib-aux (- n 1) b (+ a b))))

(define (fib n)
  (fib-aux n 0 1))

(fib 21)
