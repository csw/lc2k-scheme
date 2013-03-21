(define (not v)
  (if v #f #t))

(define (zero? v)
  (if (%zero? v) #t #f))

(define (negative? v)
  (if (%zero? (bitwise-and v %sign-bit)) #f #t))

(define (positive? v)
  (if (or (zero? v)
          (not (zero? (bitwise-and v %sign-bit))))
      #f
      #t))

(define (even? v)
  (zero? (bitwise-and v 1)))

(define (odd? v)
  (not (zero? (bitwise-and v 1))))

(define (boolean? v)
  (if (%tagged? %type-tag-mask %bool-tag v) #t #f))

(define (char? v)
  (if (%tagged? %type-tag-mask %char-tag v) #t #f))

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

;; (define (%multiply mcand mplier acc mask)
;;   (if (= mask %max-fixnum-bit)
;;       acc
;;       (%multiply (+ mcand mcand)
;;                  mplier
;;                  (if (eq? (%nand mplier mask) %all-ones)
;;                      acc
;;                      (+ acc mcand))
;;                  (+ mask mask))))

;; (define (* mcand mplier)
;;   (if (or (zero? mcand)
;;           (zero? mplier))
;;       0
;;       (%multiply mcand mplier 0 1)))
