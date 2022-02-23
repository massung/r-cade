#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)
(require ffi/vector)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(require "input.rkt")
(require "video.rkt")
(require "palette.rkt")
(require "font.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define bitmask-texture #f)


;; ----------------------------------------------------

(define (make-sprite-masks)
  (let ([image (GenImageColor 8 256 BLANK)])
    (for* ([y (range 256)]
           [x (range 8)])
      (when (odd? (arithmetic-shift y (- x 7)))
        (ImageDrawPixel image x y WHITE)))

    ; create the texture from the bitmap image
    (set! bitmask-texture (LoadTextureFromImage image))))

;; ----------------------------------------------------

(define bitmask-recs
  (for/vector ([y (range 256)])
    (make-Rectangle 0.0 (exact->inexact y) 8.0 1.0)))

;; ----------------------------------------------------

(define draw-color 0)

;; ----------------------------------------------------

(define (cls [c 0])
  (ClearBackground (vector-ref (palette) (bitwise-and c #xf))))

;; ----------------------------------------------------

(define (color n)
  (set! draw-color (vector-ref (palette) (bitwise-and n #xf))))

;; ----------------------------------------------------

(define (draw x y sprite)
  (for ([byte sprite] [i (range y (height))] #:when (>= i 0))
    (let ([p (make-Vector2 (real->double-flonum x)
                           (real->double-flonum i))]

          ; bitmask scanline
          [r (vector-ref bitmask-recs (bitwise-and byte #xff))])

      ; render the sprite for this scanline
      (DrawTextureRec bitmask-texture r p draw-color))))

;; ----------------------------------------------------

(define (draw-ex x y sprite)
  (for ([word sprite] [i (range y (height))] #:when (>= i 0))
    (let* ([b0 (bitwise-bit-field word 8 16)]
           [b1 (bitwise-bit-field word 0 8)]

           ; position vectors
           [p0 (make-Vector2 (real->double-flonum (round x))
                             (real->double-flonum (round i)))]
           [p1 (make-Vector2 (real->double-flonum (round (+ x 8)))
                             (real->double-flonum (round i)))]

           ; bitmask scanlines
           [r0 (vector-ref bitmask-recs b0)]
           [r1 (vector-ref bitmask-recs b1)])

      ; render bytes
      (DrawTextureRec bitmask-texture r0 p0 draw-color)
      (DrawTextureRec bitmask-texture r1 p1 draw-color))))

;; ----------------------------------------------------

(define (text x y s)
  (for ([i (range x (width) (font-advance))] [c (~a s)])
    (let ([sprite (font-sprite c)])
      (when sprite
        (draw i y sprite)))))

;; ----------------------------------------------------

(define (line x1 y1 x2 y2)
  (DrawLine x1 y1 x2 y2 draw-color))

;; ----------------------------------------------------

(define (rect x y w h #:fill [fill #f])
  (if fill
      (DrawRectangle x y w h draw-color)
      (DrawRectangleLines x y w h draw-color)))

;; ----------------------------------------------------

(define (circle x y r #:fill [fill #f])
  (if fill
      (DrawCircle x y r draw-color)
      (DrawCircleLines x y r draw-color)))
