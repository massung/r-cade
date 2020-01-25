#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)
(require ffi/vector)
(require csfml)

;; ----------------------------------------------------

(require "video.rkt")
(require "palette.rkt")
(require "font.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define bitmask-texture
  (let ([image (sfImage_create 8 256)])
    (for ([i (range 256)])
      (let ([pixel (λ (b)
                     (let ([set? (odd? (arithmetic-shift i (- b)))])
                       (if set? sfWhite sfTransparent)))])
        (sfImage_setPixel image 0 i (pixel 7))
        (sfImage_setPixel image 1 i (pixel 6))
        (sfImage_setPixel image 2 i (pixel 5))
        (sfImage_setPixel image 3 i (pixel 4))
        (sfImage_setPixel image 4 i (pixel 3))
        (sfImage_setPixel image 5 i (pixel 2))
        (sfImage_setPixel image 6 i (pixel 1))
        (sfImage_setPixel image 7 i (pixel 0))))

    ; create the texture from the bitmap image
    (sfTexture_createFromImage image #f)))

;; ----------------------------------------------------

(define bitmask-sprite (sfSprite_create))

;; ----------------------------------------------------

(define bitmask-rects
  (for/vector ([y (range 256)])
    (make-sfIntRect 0 y 8 1)))

;; ----------------------------------------------------

(define rect-shape (sfRectangleShape_create))
(define circle-shape (sfCircleShape_create))
(define line-shape (sfVertexArray_create))

;; ----------------------------------------------------

(begin
  (sfSprite_setTexture bitmask-sprite bitmask-texture #f)

  ; initialize primitives
  (sfVertexArray_resize line-shape 2)
  (sfVertexArray_setPrimitiveType line-shape 'sfLines))

;; ----------------------------------------------------

(define (set-vertex! prim n c x y)
  (let ([v (sfVertexArray_getVertex prim n)])
    (set-sfVertex-color! v c)

    ; update the x/y coordinate
    (set-sfVector2f-x! (sfVertex-position v) (real->single-flonum x))
    (set-sfVector2f-y! (sfVertex-position v) (real->single-flonum y))))

;; ----------------------------------------------------

(define (cls)
  (sfRenderTexture_clear (texture) (vector-ref palette 0)))

;; ----------------------------------------------------

(define (color n)
  (let ([c (vector-ref palette (bitwise-and n #xf))])
    (sfSprite_setColor bitmask-sprite c)))

;; ----------------------------------------------------

(define (draw x y sprite)
  (for ([byte sprite] [i (range y (height))] #:when (>= i 0))
    (let ([p (make-sfVector2f (real->single-flonum x)
                              (real->single-flonum i))]

          ; bitmask scanline
          [r (vector-ref bitmask-rects (bitwise-and byte #xff))])
      (sfSprite_setTextureRect bitmask-sprite r)
      (sfSprite_setPosition bitmask-sprite p))

    ; render the sprite for this scanline
    (sfRenderTexture_drawSprite (texture) bitmask-sprite #f)))

;; ----------------------------------------------------

(define (text x y s)
  (for ([i (range x (width) 4)] [ch (~a s)])
    (let ([n (char->integer ch)])
      (when (<= 33 n 127)
        (draw i y (vector-ref font (- n 33)))))))

;; ----------------------------------------------------

(define (line x1 y1 x2 y2)
  (let ([c (sfSprite_getColor bitmask-sprite)])
    (set-vertex! line-shape 0 c x1 y1)
    (set-vertex! line-shape 1 c x2 y2)

    ; draw the line
    (sfRenderTexture_drawVertexArray (texture) line-shape #f)))

;; ----------------------------------------------------

(define (rect x y w h #:fill [fill #f])
  (let ([c (sfSprite_getColor bitmask-sprite)]
        [p (make-sfVector2f (real->single-flonum x)
                            (real->single-flonum y))]
        [s (make-sfVector2f (real->single-flonum w)
                            (real->single-flonum h))])
    (sfRectangleShape_setSize rect-shape s)
    (sfRectangleShape_setPosition rect-shape p)
    (sfRectangleShape_setOutlineColor rect-shape c)
    (sfRectangleShape_setOutlineThickness rect-shape 1.0)
    (sfRectangleShape_setFillColor rect-shape (if fill c sfTransparent))

    ; draw the rectangle
    (sfRenderTexture_drawRectangleShape (texture) rect-shape #f)))

;; ----------------------------------------------------

(define (circle x y r #:fill [fill #f])
  (let ([c (sfSprite_getColor bitmask-sprite)]
        [p (make-sfVector2f (real->single-flonum x)
                            (real->single-flonum y))])
    (sfCircleShape_setRadius circle-shape (real->single-flonum r))
    (sfCircleShape_setPosition circle-shape p)
    (sfCircleShape_setOutlineColor circle-shape c)
    (sfCircleShape_setOutlineThickness circle-shape 1.0)
    (sfCircleShape_setFillColor circle-shape (if fill c sfTransparent))

    ; draw the circle
    (sfRenderTexture_drawCircleShape (texture) circle-shape #f)))
