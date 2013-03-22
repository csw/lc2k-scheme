#lang racket

(provide tagged-mask tagged-tag type-tag-mask no-type-tag-mask
         tag-shift pointer-mask constant-bit
         char-tag bool-tag empty-tag cons-tag proc-tag
         true-v false-v empty-list-v
         decode-immediate immediate-rep)

;;;; Type tags

;; Two high bits are reserved to distinguish fixnums from other
;; types. A word whose two most significant bits are 01 is an
;; immediate Scheme value or a tagged pointer.
;;
;; Immediates:
;;   fixnums, -2^31 to 2^30 - 1
;;   booleans: #t and #f
;;   chars
;;   empty (the empty list)
;;
;; Pointer types:
;;   conses
;;   procedures
;;
;; Unimplemented:
;;   vectors
;;   symbols
;;   strings
;;
;; Tag byte (MSB):
;; 01pttttc
;;   p: pointer bit
;;   t: 4 tag bits
;;   c: constant pointer bit
;;
;;
;; bits 31-30: 01 for tagged Scheme types, otherwise fixnum
;;             => tag byte: tag-bits | 0x40
;; bit  29:    pointer (1) or immediate (0)
;;             => tag byte: tag-bits | 0x20
;; bits 28-25: type tag for immediate or pointer
;;
;; immediate type tags (28-25): (with hex tag byte)
;; 0000 char        0x40
;; 0001 bool        0x42
;; 0010 empty       0x44
;;
;; pointer type tags (28-25)
;; 0000 cons        0x60 / 0x61
;; 0001 vector      0x62 / 0x63
;; 0010 string      0x64
;; 0011 symbol      0x66
;; 0100 procedure   0x68
;; 0101 closure     0x6A
;; 0110 environment 0x6C
;;
;; For pointers:
;;   bit 24: constant pointer

(define (type-tag bits)
  (bitwise-ior tagged-tag
               (arithmetic-shift bits tag-shift)))

(define (immed-type-tag bits)
  (type-tag bits))

(define (pointer-type-tag bits)
  (type-tag (bitwise-ior (expt 2 4) bits)))

(define tagged-mask  #b11000000000000000000000000000000)
(define tagged-tag   #b01000000000000000000000000000000)
(define type-tag-mask #xFE000000)
(define no-type-tag-mask (bitwise-not type-tag-mask))
(define tag-shift    25)
(define char-tag     (immed-type-tag #b0000))
(define bool-tag     (immed-type-tag #b0001))
(define true-v       (add1 bool-tag))
(define false-v      bool-tag)
(define empty-tag    (immed-type-tag #b0010))
(define empty-list-v empty-tag)
(define pointer-mask #x0000FFFF)
(define cons-tag     (pointer-type-tag #b0000))
(define proc-tag     (pointer-type-tag #b0100))
(define constant-bit (expt 2 24))

(define (tag-bits v)
  (bitwise-and v type-tag-mask))

(define (decode-immediate v)
  (if (= tagged-tag
         (bitwise-and tagged-mask v))
      (let ([tag (tag-bits v)])
        (cond
         [(= v true-v) #t]
         [(= v false-v) #f]
         [(= v empty-list-v) empty]
         [(= tag char-tag) (integer->char (bitwise-and v no-type-tag-mask))]
         [else (raise-argument-error 'decode-immediate "immediate" v)]))
      ;; fixnum
      v))

(define (immediate-rep v)
  (cond
   [(integer? v) v]
   [(char? v) (bitwise-ior (char->integer v) char-tag)]
   [(boolean? v) (match v
                   [#t true-v]
                   [#f false-v])]
   [(empty? v) empty-list-v]
   [#t (raise-argument-error 'immediate-rep "immediate" v)]))

