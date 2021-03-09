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

(define framerate (make-parameter #f))
(define frame (make-parameter #f))
(define frameclock (make-parameter #f))

;; ----------------------------------------------------

(define frametime (make-parameter #f))
(define gametime (make-parameter #f))

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

(define (process-frametime)
  (let* ([elapsed (sfClock_getElapsedTime (frameclock))]
         [delta (- (/ (framerate)) (sfTime_asSeconds elapsed))])
    (unless (< delta 0.0)
      (sleep delta)))

  ; update frametime, gametime, frame, and reset the frame clock
  (frametime (sfTime_asSeconds (sfClock_getElapsedTime (frameclock))))
  (gametime (+ (gametime) (frametime)))
  (frame (+ (frame) 1))
  (sfClock_restart (frameclock)))
