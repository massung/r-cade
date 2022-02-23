#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require racket/match)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(require "time.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define mouse-x (make-parameter #f))
(define mouse-y (make-parameter #f))

;; ----------------------------------------------------

(define (update-mouse-pos)
  (mouse-x (GetMouseX))
  (mouse-y (GetMouseY)))

;; ----------------------------------------------------

(define-syntax (define-btn stx)
  (syntax-case stx ()
    [(_ btn id pressed? down?)
     #'(define (btn [hold? #f])
         ((if hold? down? pressed?) id))]))

;; ----------------------------------------------------

(define-btn btn-start 'KEY_ENTER IsKeyPressed IsKeyDown)
(define-btn btn-select 'KEY_SPACE IsKeyPressed IsKeyDown)
(define-btn btn-quit 'KEY_ESCAPE IsKeyPressed IsKeyDown)
(define-btn btn-z 'KEY_Z IsKeyPressed IsKeyDown)
(define-btn btn-x 'KEY_X IsKeyPressed IsKeyDown)
(define-btn btn-up 'KEY_UP IsKeyPressed IsKeyDown)
(define-btn btn-down 'KEY_DOWN IsKeyPressed IsKeyDown)
(define-btn btn-right 'KEY_RIGHT IsKeyPressed IsKeyDown)
(define-btn btn-left 'KEY_LEFT IsKeyPressed IsKeyDown)

;; ----------------------------------------------------

(define-btn btn-mouse 'MOUSE_BUTTON_LEFT IsMouseButtonPressed IsMouseButtonDown)

;; ----------------------------------------------------

(define (btn-any)
  (or (btn-start)
      (btn-select)
      (btn-quit)
      (btn-z)
      (btn-x)
      (btn-mouse)))

;; ----------------------------------------------------

(define (action pred [rate #f])
  (let ([last #f])
    (λ ()
      (let ([time (GetTime)]
            [pressed? (pred rate)])
        (and pressed? (cond
                        [(or (not last) (boolean? rate))
                         (begin0 #t (set! last time))]

                        ;; has enough time elapsed since the last fire?
                        [(> (- time last) (/ 1.0 rate))
                         (begin0 #t (set! last time))]

                        ;; not enough time elapsed
                        [else #f]))))))

;; ----------------------------------------------------

#;(define (on-mouse-moved event)
  (let* ([x (sfMouseMoveEvent-x event)]
         [y (sfMouseMoveEvent-y event)]

         ; window coordinate
         [pixel (make-sfVector2i x y)]

         ; convert to game coordinate
         [coord (sfRenderWindow_mapPixelToCoords (window) pixel #f)]

         ; inverse transform of sprite
         [inv (sfSprite_getInverseTransform (sprite))]
         [point (sfTransform_transformPoint inv coord)])
    (mouse-x (inexact->exact (floor (sfVector2f-x point))))
    (mouse-y (inexact->exact (floor (sfVector2f-y point))))))
