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

(define camera (make-Camera2D (Vector2Zero) (Vector2Zero) 0.0 1.0))

;; ----------------------------------------------------

(define (update-camera)
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
         [y (* h 0.5)])

    ; top-left offset to render at
    (set-Vector2-x! (Camera2D-offset camera) (- (/ screen-w 2) x))
    (set-Vector2-y! (Camera2D-offset camera) (- (/ screen-h 2) y))

    ; scale factor
    (set-Camera2D-zoom! camera scale)))

;; ----------------------------------------------------

(define (flip frame time)
  (BeginMode2D camera)
  (ClearBackground bg)
  (let ([texture (RenderTexture2D-texture (target))])
    (BeginShaderMode (shader))
    (DrawTexture texture 0 0 WHITE)
    (EndShaderMode))
  (EndMode2D)
  (EndDrawing))
