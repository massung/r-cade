#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)
(require racket/match)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(require "shader.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define window (make-parameter #f))
(define target (make-parameter #f))
(define width (make-parameter #f))
(define height (make-parameter #f))
(define shader (make-parameter #f))

;; ----------------------------------------------------

(define bg (make-Color #x10 #x10 #x18 #xff))

;; ----------------------------------------------------

(define hide-mouse HideCursor)
(define show-mouse ShowCursor)

;; ----------------------------------------------------

(define uniform-time (malloc _float))

;; ----------------------------------------------------

(define (flip frame time)
  (let* ([screen-w (GetScreenWidth)]
         [screen-h (GetScreenHeight)]

         ; scale factor for each axis
         [sx (real->double-flonum (/ screen-w (width)))]
         [sy (real->double-flonum (/ screen-h (height)))]

         ; uniform scaling factor
         [scale (max (floor (min sx sy)) 1.0)]

         ; width of target
         [w (* (width) scale)]
         [h (* (height) scale)]
         
         ; center of the display
         [x (* w 0.5)]
         [y (* h 0.5)]

         ; source rectangle - flip y
         [source (make-Rectangle 0.0 0.0 w (- h))]

         ; destination rectangle
         [pos (make-Vector2 (- (/ screen-w 2) x) (- (/ screen-h 2) y))])

    ; prepare the render sprite
    (BeginDrawing)
    (ClearBackground bg)
    (let ([texture (RenderTexture2D-texture (target))])
      (BeginShaderMode (shader))
      (DrawTextureEx texture pos 0.0 scale WHITE)
      (EndShaderMode))
    (EndDrawing)))
