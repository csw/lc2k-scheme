#lang racket

(require "util.rkt")

(provide expand-prims transform-program)

;; convert to our labels / code representation
(define (transform-program prog)
  (let*-values ([(defs rest) (partition (lambda (e)
                                          (tagged-list? 'define e))
                                        (if (list? prog)
                                            prog
                                            (list prog)))]
                [(label-defs) (for/list ([def defs])
                                (match def
                                  [(list 'define
                                         (list name formals ...)
                                         body)
                                   `(,name (code ,formals ,body))]))]
                [(toplevel) (if (and (pair? rest) (empty? (cdr rest)))
                                (car rest)
                                rest)])
    `(labels ,label-defs ,toplevel)))

(define primitives
  '(+ %nand %and %eq? %zero?))

(define prim-predicates
  '(%eq? %zero?))

(define (prim? exp)
  (member exp primitives))

(define (prim-predicate? exp)
  (member exp prim-predicates))

(define (ref? exp)
  (symbol? exp))

;; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

;; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

;; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

;; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))
(define (app? exp)
  (pair? exp))

(define (expand-primcall exp)
  (let ([expanded
         (match exp
           [(list '%tagged? mask tag v)
            `(%eq? (primcall %band
                             ,(expand-prims mask)
                             ,(expand-prims v))
                   ,(expand-prims tag))]
           [(list '= x y)           `(%eq? ,(expand-prims x)
                                           ,(expand-prims y))]
           [(list '+ x y)           `(%add ,(expand-prims x)
                                           ,(expand-prims y))]
           [(list '- x)             `(%add 1
                                           ,(expand-prims
                                             (list 'bitwise-not x)))]
           [(list '- x (? number? y))      `(%add ,(expand-prims x)
                                           ,(- y))]
           [(list '- x y)           `(%add ,(expand-prims x)
                                           ,(expand-prims
                                             (list '- y)))]
           [(list 'bitwise-and x y) `(%band ,(expand-prims x)
                                            ,(expand-prims y))]
           [(list 'bitwise-ior x y) `(%nand ,(expand-prims `(bitwise-not ,x))
                                           ,(expand-prims `(bitwise-not ,y)))]
           [(list 'bitwise-not x)  `(%bnot ,(expand-prims x))]
           [else #f])])
    (if expanded
        (cons 'primcall (or (expand-primcall expanded) expanded))
        #f)))

(define (expand-call exp)
  (or (expand-primcall exp)
      (cons 'call
            (cons (car exp)
                  (map expand-prims (cdr exp))))))

(define (expand-if-pred exp)
  (match exp
    [(list (? prim-predicate? pred) args ...)
     (list* 'primcall pred (map expand-prims args))]
    [(list 'zero? v)
     (list 'primcall '%zero? (expand-prims v))]
    [(list 'empty? v)
     (list 'primcall '%eq? (expand-prims v) empty)]
    ;; and, or
    [(list (and op (or 'and 'or)) args ...)
     (list* op (map expand-if-pred args))]
    [else
     ;; XXX: review all this
     (let ([expanded (expand-prims exp)])
       (match expanded
         ;; eq? or zero?
         [(list-rest 'primcall (? prim-predicate?) ... args) expanded]
         [else `(not (primcall %eq? #f ,expanded))]))]))

(define (expand-prims exp)
  (match exp
   ;; Core forms:
   [(list 'code (list args ...) body-exprs ...)
    `(code ,args ,@(map expand-prims body-exprs))]
   [(? immed-const?)      exp]
   [(? prim?)       exp]
   [(? ref?)        exp]
   [(? if?)         `(if ,(expand-if-pred (if->condition exp))
                           ,(expand-prims (if->then exp))
                           ,(expand-prims (if->else exp)))]
   [(list (and (or 'and 'or)
               op)
          args ..1)         `(,op ,@(map expand-prims args))]
   [(? app?)        (expand-call exp)]
   [else              (error "unknown exp: " exp)]))

