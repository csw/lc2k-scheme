(define (zero? v)
  (if (%zero? v) #t #f))

(define (boolean? v)
  (if (%tagged? %tag-mask %bool-tag v) #t #f))

(define (char? v)
  (if (%tagged? %tag-mask %char-tag v) #t #f))

(define (not v)
  (if v #f #t))
