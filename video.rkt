#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)
(require racket/match)

;; ----------------------------------------------------

(require "shader.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define window (make-parameter #f))
(define texture (make-parameter #f))
(define sprite (make-parameter #f))
(define width (make-parameter #f))
(define height (make-parameter #f))
(define shader (make-parameter #f))
(define render-state (make-parameter #f))

;; ----------------------------------------------------

(define bg (sfColor_fromRGBA #x10 #x10 #x18 #xff))

;; ----------------------------------------------------

(define (hide-mouse)
  (sfRenderWindow_setMouseCursorVisible (window) #f))

;; ----------------------------------------------------

(define (show-mouse)
  (sfRenderWindow_setMouseCursorVisible (window) #t))

;; ----------------------------------------------------

(define (resize event)
  (let ([view (sfRenderWindow_getView (window))]

        ; new size from event
        [w (real->single-flonum (sfSizeEvent-width event))]
        [h (real->single-flonum (sfSizeEvent-height event))])

    ; update the view to ensure the sprite is centered
    (sfView_setSize view (make-sfVector2f w h))
    (sfRenderWindow_setView (window) view)))

;; ----------------------------------------------------

(define (flip frame time)
  (sfRenderTexture_display (texture))

  ; update the sprite
  (let* ([view (sfRenderWindow_getView (window))]
         [size (sfRenderWindow_getSize (window))]

         ; dimensions of the window
         [w (sfVector2u-x size)]
         [h (sfVector2u-y size)]

         ; scale factor for each axis
         [sx (real->single-flonum (/ w (width)))]
         [sy (real->single-flonum (/ h (height)))]

         ; square scaling factor
         [base (min sx sy)]
         [scale (if (< base 1.0) base (floor base))]

         ; center of the display
         [x (* (width) 0.5)]
         [y (* (height) 0.5)]

         ; find the center pixel of the window
         [center (make-sfVector2i (quotient w 2) (quotient h 2))])
    
    ; setup the display to render in the center
    (sfSprite_setTexture (sprite) (sfRenderTexture_getTexture (texture)) #t)
    (sfSprite_setOrigin (sprite) (make-sfVector2f x y))
    (sfSprite_setScale (sprite) (make-sfVector2f scale scale))
    (sfSprite_setPosition (sprite) (sfRenderWindow_mapPixelToCoords (window) center #f)))

  ; use the crt shader (may be null)
  (set-sfRenderStates-shader! (render-state) (shader))
  
  ; redraw the window
  (sfRenderWindow_clear (window) bg)
  (sfRenderWindow_drawSprite (window) (sprite) (render-state))
  
  (sfRenderWindow_display (window)))
