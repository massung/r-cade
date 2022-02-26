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

(define frame (make-parameter #f))
(define paused (make-parameter #f))

;; ----------------------------------------------------

(define framerate GetFPS)
(define gametime GetTime)

;; ----------------------------------------------------

(define (frametime)
  (if (paused) 0 (GetFrameTime)))

;; ----------------------------------------------------

(define (timer time #:loop [loop #f])
  (let ([this time])
    (λ (#:reset [reset #f])
      (set! this (if reset time (- this (frametime))))

      ; true if the this time has expired
      (let ([expired (< this 0.0)])
        (begin0 expired

                ; reset if looping
                (when (and loop expired)
                  (set! this time)))))))

;; ----------------------------------------------------

(define (update-frame)
  (unless (paused)
    (frame (+ (frame) 1))))
