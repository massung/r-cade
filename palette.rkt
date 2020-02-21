#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define palette (make-parameter #f))

;; ----------------------------------------------------

(define basic-palette
  (vector (sfColor_fromRGBA #x00 #x00 #x00 #xff)
          (sfColor_fromRGBA #x1d #x2b #x53 #xff)
          (sfColor_fromRGBA #x7e #x25 #x53 #xff)
          (sfColor_fromRGBA #x00 #x87 #x51 #xff)
          (sfColor_fromRGBA #xab #x52 #x36 #xff)
          (sfColor_fromRGBA #x5f #x57 #x4f #xff)
          (sfColor_fromRGBA #xc2 #xc3 #xc7 #xff)
          (sfColor_fromRGBA #xff #xf1 #xe8 #xff)
          (sfColor_fromRGBA #xff #x00 #x4d #xff)
          (sfColor_fromRGBA #xff #xa3 #x00 #xff)
          (sfColor_fromRGBA #xff #xec #x27 #xff)
          (sfColor_fromRGBA #x00 #xe4 #x36 #xff)
          (sfColor_fromRGBA #x29 #xad #xff #xff)
          (sfColor_fromRGBA #x83 #x76 #x9c #xff)
          (sfColor_fromRGBA #xff #x77 #xab #xff)
          (sfColor_fromRGBA #xff #xcc #xaa #xff)))

;; ----------------------------------------------------

(define (set-color! index red green blue)
  (let ([n (bitwise-and index #x0f)]
        [r (bitwise-and red   #xff)]
        [g (bitwise-and green #xff)]
        [b (bitwise-and blue  #xff)])
    (vector-set! (palette) n (sfColor_fromRGBA r g b #xff))))
