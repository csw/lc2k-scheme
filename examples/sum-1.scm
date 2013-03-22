(define (sum l acc)
  (if (empty? l)
      acc
      (sum (cdr l)
           (+ (car l) acc))))

(sum (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
     0)
