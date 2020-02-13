#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)
(require racket/match)

;; ----------------------------------------------------

(require "video.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define buttons (make-parameter #f))

;; ----------------------------------------------------

(define mouse-x (make-parameter #f))
(define mouse-y (make-parameter #f))

;; ----------------------------------------------------

(define (update-buttons)
  (for ([(k v) (buttons)] #:when v)
    (hash-set! (buttons) k (+ v 1))))

;; ----------------------------------------------------

(define (btn? b)
  (hash-ref (buttons) b #f))

;; ----------------------------------------------------

(define (btn-start) (btn? 'sfKeyEnter))
(define (btn-select) (btn? 'sfKeySpace))
(define (btn-quit) (btn? 'sfKeyEscape))
(define (btn-z) (btn? 'sfKeyZ))
(define (btn-x) (btn? 'sfKeyX))
(define (btn-up) (btn? 'sfKeyUp))
(define (btn-down) (btn? 'sfKeyDown))
(define (btn-right) (btn? 'sfKeyRight))
(define (btn-left) (btn? 'sfKeyLeft))
(define (btn-mouse) (btn? 'sfMouseLeft))

;; ----------------------------------------------------

(define (btn-any)
  (or (btn-start)
      (btn-select)
      (btn-quit)
      (btn-z)
      (btn-x)))  

;; ----------------------------------------------------

(define (press-button btn)
  (let ([update (λ (hits)
                  (if hits hits 1))])
    (hash-update! (buttons) btn update #f)))

;; ----------------------------------------------------

(define (release-button btn)
  (hash-set! (buttons) btn #f))

;; ----------------------------------------------------

(define (on-key-pressed event)
  (press-button (sfKeyEvent-code event)))

;; ----------------------------------------------------

(define (on-key-released event)
  (release-button (sfKeyEvent-code event)))

;; ----------------------------------------------------

(define (on-mouse-moved event)
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

;; ----------------------------------------------------

(define (on-mouse-clicked event)
  (press-button (sfMouseButtonEvent-button event)))

;; ----------------------------------------------------

(define (on-mouse-released event)
  (release-button (sfMouseButtonEvent-button event)))
