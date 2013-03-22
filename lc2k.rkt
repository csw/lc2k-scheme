#lang racket

(provide sp-reg call-reg spill-reg ret-reg rv-reg
         spill-dest-reg spill-s1-reg spill-s2-reg)

;; LC2K machine definition

;; The LC2K has 8 registers, used as follows.
;; 
;; r0: always 0
;; r1: return value, first argument
;; r2: second argument
;; r3: third argument
;; r4: scratch
;; r5: scratch
;; r6: return address
;; r7: stack pointer
;;
;; The arguments and return address may be spilled to the stack and
;; their registers reused.
;;
;; r4 and r5 are reserved for loading and storing spilled variables.

(define sp-reg 7)
(define call-reg 5)
(define spill-reg 5)
(define ret-reg 6)
(define rv-reg 1)

(define spill-dest-reg 5)
(define spill-s1-reg 4)
(define spill-s2-reg 5)

