#lang racket

(require r-cade)

;; ----------------------------------------------------

(define strings
  '("!\"#$%&'()*+,-./"
    "0123456789"
    ":;<=>?@"
    "ABCDEFGHIJKLM"
    "NOPQRSTUVWXYZ"
    "[\\]^_`"
    "abcdefghijklm"
    "nopqrstuvwxyz"
    "{|}~"))

;; ----------------------------------------------------

(define (test-font)
  (cls)
  (color 7)

  ; set the font based on the button
  (font (cond
          ((btn-z) tall-font)
          ((btn-x) wide-font)
          (#t basic-font)))

  ; draw strings
  (for ([s strings] [y (in-naturals)])
    (text 1 (+ (* y (font-height)) 1) s)))

;; ----------------------------------------------------

(module+ main
  (run test-font 128 90))
