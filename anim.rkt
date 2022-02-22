#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require raylib)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define (anim-frame sprites time #:fps [fps 8] #:loop [loop #t])
  (let ([m (sequence-length sprites)])
    (remainder (exact-floor (* time fps)) m)))

;; ----------------------------------------------------

(define (anim-sprite sprites time #:fps [fps 8] #:loop [loop #t])
  (sequence-ref sprites (anim-frame sprites time #:fps fps #:loop loop)))
 