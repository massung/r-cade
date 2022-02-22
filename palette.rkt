#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/cvector)
(require ffi/unsafe)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define palette (make-parameter #f))

;; ----------------------------------------------------

(define basic-palette
  (vector (make-Color #x00 #x00 #x00 #xff)
          (make-Color #x1d #x2b #x53 #xff)
          (make-Color #x7e #x25 #x53 #xff)
          (make-Color #x00 #x87 #x51 #xff)
          (make-Color #xab #x52 #x36 #xff)
          (make-Color #x5f #x57 #x4f #xff)
          (make-Color #xc2 #xc3 #xc7 #xff)
          (make-Color #xff #xf1 #xe8 #xff)
          (make-Color #xff #x00 #x4d #xff)
          (make-Color #xff #xa3 #x00 #xff)
          (make-Color #xff #xec #x27 #xff)
          (make-Color #x00 #xe4 #x36 #xff)
          (make-Color #x29 #xad #xff #xff)
          (make-Color #x83 #x76 #x9c #xff)
          (make-Color #xff #x77 #xab #xff)
          (make-Color #xff #xcc #xaa #xff)))

;; ----------------------------------------------------

(define (set-color! index red green blue)
  (let ([n (bitwise-and index #x0f)]
        [r (bitwise-and red   #xff)]
        [g (bitwise-and green #xff)]
        [b (bitwise-and blue  #xff)])
    (vector-set! (palette) n (make-Color r g b #xff))))
