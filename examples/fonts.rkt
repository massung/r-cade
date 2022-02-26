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

(define show-font 0)

;; ----------------------------------------------------

(define select-font (action btn-select))

;; ----------------------------------------------------

(define (test-font)
  (cls)
  (color 7)

  ; cycle through fonts with select
  (when (select-font)
    (set! show-font (remainder (+ show-font 1) 3)))

  ; current font
  (font (case show-font
          [(0) basic-font]
          [(1) tall-font]
          [(2) wide-font]))

  ; draw strings
  (for ([s strings] [y (in-naturals)])
    (text 1 (+ (* y (font-height)) 1) s)))

;; ----------------------------------------------------

(module+ main
  (run test-font 128 90))
