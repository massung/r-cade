#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)
(require racket/match)

;; ----------------------------------------------------

(require "time.rkt")
(require "video.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define buttons (make-parameter #f))

;; ----------------------------------------------------

(define mouse-x (make-parameter #f))
(define mouse-y (make-parameter #f))

;; ----------------------------------------------------

(define (btn? b [hold #f] [rate #f])
  (let ([x (hash-ref (buttons) b #f)])
    (and x (let ([frames-held (- (frame) x 1)])
             (cond
               [(zero? frames-held) #t]

               ; allow button hold, inifite repeat
               [(not rate) hold]

               ; has it been held for a multiple of the rate?
               [else (let ([n (quotient (framerate) rate)])
                       (zero? (remainder frames-held n)))])))))

;; ----------------------------------------------------

(define (btn-start [hold #f] [rate #f]) (btn? 'sfKeyEnter hold rate))
(define (btn-select [hold #f] [rate #f]) (btn? 'sfKeySpace hold rate))
(define (btn-quit [hold #f] [rate #f]) (btn? 'sfKeyEscape hold rate))
(define (btn-break [hold #f] [rate #f]) (btn? 'sfKeyF8 hold rate))
(define (btn-z [hold #f] [rate #f]) (btn? 'sfKeyZ hold rate))
(define (btn-x [hold #f] [rate #f]) (btn? 'sfKeyX hold rate))
(define (btn-up [hold #f] [rate #f]) (btn? 'sfKeyUp hold rate))
(define (btn-down [hold #f] [rate #f]) (btn? 'sfKeyDown hold rate))
(define (btn-right [hold #f] [rate #f]) (btn? 'sfKeyRight hold rate))
(define (btn-left [hold #f] [rate #f]) (btn? 'sfKeyLeft hold rate))
(define (btn-mouse [hold #f] [rate #f]) (btn? 'sfMouseLeft hold rate))

;; ----------------------------------------------------

(define (btn-any)
  (or (btn-start)
      (btn-select)
      (btn-quit)
      (btn-z)
      (btn-x)))

;; ----------------------------------------------------

(define (action btn [hold #f] [rate #f])
  (λ () (btn hold rate)))

;; ----------------------------------------------------

(define (press-button btn [reset #f])
  (let ([update (λ (since-frame)
                  (or (and (not reset) since-frame) (frame)))])
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
