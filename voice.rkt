#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(struct voice [instrument envelope])

;; ----------------------------------------------------

(define (fmod x d)
  (let* ([n (* d (truncate (/ x d)))]
         [m (- x n)])
    (if (< m pi) m (- m d))))

;; ----------------------------------------------------

(define sine-wave sin)

;; ----------------------------------------------------

(define (sawtooth-wave x)
  (let ([s (fmod x (* pi 2))])
    (max (min (/ s pi) 1.0) -1.0)))

;; ----------------------------------------------------

(define (square-wave x)
  (if (> (sin x) 0.5) 1.0 0.0))

;; ----------------------------------------------------

(define (triangle-wave x)
  (let* ([p (* pi 2.0)]
         [f (floor (+ (/ (* x 2) p) 1/2))])
    (* (/ 4 p) (- x (* (/ p 2) f)) (expt -1 f))))

;; ----------------------------------------------------

(define (noise-wave x)
  (square-wave (random)))

;; ----------------------------------------------------

(define (envelope y . ys)
  (if (null? ys)
      (const y)

      ; given [0,1] range, linear interpolate
      (let* ([ys (list->vector (cons y ys))]
             [n (- (vector-length ys) 1)])
        (λ (s)
          (cond
            [(<= s 0.0) y]

            ; use last point
            [(>= s 1.0) (vector-ref ys n)]

            ; linearly interpolate from u0 -> u1
            [else (let* ([u (inexact->exact (floor (* s n)))]
                         [t (- (* s n) u)]

                         ; from y0 -> y1
                         [y0 (vector-ref ys u)]
                         [y1 (vector-ref ys (+ u 1))])
                    (+ y0 (* t (- y1 y0))))])))))

;; ----------------------------------------------------

(define z-envelope (envelope 1 1 0 0))

;; ----------------------------------------------------

(define s-envelope (envelope 0 0 1 1))

