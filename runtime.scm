(define (zero? v)
  (if (%zero? v) #t #f))

(define (boolean? v)
  (if (%tagged? %type-tag-mask %bool-tag v) #t #f))

(define (char? v)
  (if (%tagged? %type-tag-mask %char-tag v) #t #f))

(define (not v)
  (if v #f #t))

(define (eq? obj1 obj2)
  (if (%eq? obj1 obj2) #t #f))

(define (empty? v)
  (if (empty? v) #t #f))

(define (null? v)
  (if (empty? v) #t #f))

(define (pair? v)
  (if (%tagged? %type-tag-mask %cons-tag v) #t #f))

(define (integer? v)
  (if (%tagged? %tagged-mask %tagged-tag v) #f #t))

(define (number? v)
  ;; synonymous with integer? for now...
  (if (%tagged? %tagged-mask %tagged-tag v) #f #t))
