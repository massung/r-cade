#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require racket/match)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(struct voice [instrument envelope])

;; ----------------------------------------------------

(define basic-voice (voice sin (const 1)))

;; ----------------------------------------------------

(define sine-wave sin)

;; ----------------------------------------------------

(define (square-wave x)
  (sgn (sin x)))

;; ----------------------------------------------------

(define (sawtooth-wave x)
  (let ([y (/ x (* 2 pi))])
    (* 2 (- y (floor (+ y 0.5))))))

;; ----------------------------------------------------

(define (triangle-wave x)
  (- (* 2 (abs (sawtooth-wave x))) 1))

;; ----------------------------------------------------

(define (noise-wave x)
  (* (square-wave x) (random)))

;; ----------------------------------------------------

(define-syntax synth
  (syntax-rules ()
    [(_ (f n) ...)
     (let ([hs (list (λ (x) (* (f x) n)) ...)])
       (λ (x)
         (let ([w (for/sum ([wave hs] [m (in-naturals 1)])
                    (wave (* x m)))])
           (min (max (* w) -1) 1))))]))

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

(define basic-envelope (const 1))

;; ----------------------------------------------------

(define fade-in-envelope (envelope 0 1))

;; ----------------------------------------------------

(define fade-out-envelope (envelope 1 0))

;; ----------------------------------------------------

(define z-envelope (envelope 1 1 0 0))

;; ----------------------------------------------------

(define s-envelope (envelope 0 0 1 1))

;; ----------------------------------------------------

(define peak-envelope (envelope 0 1 0))

;; ----------------------------------------------------

(define trough-envelope (envelope 1 0 1))

;; ----------------------------------------------------

(define adsr-envelope (envelope 0 1 0.7 0.7 0))
