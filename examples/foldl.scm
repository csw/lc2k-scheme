(define (foldl proc init lst)
  (if (empty? lst)
      init
      (foldl proc
             (proc init (car lst))
             (cdr lst))))

(foldl +
       0
       (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
