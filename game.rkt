#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe/custodian)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(require "video.rkt")
(require "shader.rkt")
(require "input.rkt")
(require "font.rkt")
(require "draw.rkt")
(require "palette.rkt")
(require "audio.rkt")
(require "time.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define should-quit (make-parameter #f))

;; ----------------------------------------------------

(define (quit)
  (should-quit #t))

;; ----------------------------------------------------

(define game-loop
  (make-parameter (λ ()
                    (cls)
                    (when (btn-quit)
                      (quit)))))

;; ----------------------------------------------------

(define goto game-loop)

;; ----------------------------------------------------

(define (sync)
  (update-frame)
  (update-mouse-pos)
  (update-music)

  ; render vram
  (flip (frame) (gametime))

  ; check for window close
  (should-quit (WindowShouldClose)))

;; ----------------------------------------------------

(define (wait [until btn-any])
  (do () [(or (until) (WindowShouldClose)) #f]
    (sync)))

;; ----------------------------------------------------

(define (run initial-game-loop
             pixels-wide
             pixels-high
             #:init [init #f]
             #:fps [fps 60]
             #:shader [effect #t]
             #:title [title "R-cade"]
             #:trace-log [log-level 'LOG_NONE])
  (InitWindow 0 0 title)
  (SetWindowState 'FLAG_WINDOW_RESIZABLE)
  (SetTargetFPS fps)
  (SetTraceLogLevel log-level)
  (SetExitKey 'KEY_NULL)

  ; create the sprite masks
  (make-sprite-masks)

  ; initialize global state
  (parameterize
      ([target (LoadRenderTexture pixels-wide pixels-high)]

       ; create a fragment shader for fullscreen rendering
       [shader (and effect (LoadShaderFromMemory #f fragment-shader))]

       ; default palette
       [palette (for/vector ([c basic-palette]) c)]

       ; default font
       [font basic-font]

       ; playfield size
       [width pixels-wide]
       [height pixels-high]

       ; mouse position
       [mouse-x 0]
       [mouse-y 0]

       ; frame
       [frame 0]

       ; no playing music
       [playing-stream #f]
       
       ; initial game state loop
       [game-loop initial-game-loop]
       [should-quit #f])

    ; attempt to close the windw on shutdown (or re-run)
    (dynamic-wind

     ; initialization
     (λ ()
       (when init
         (init))
       
       ; defaults
       (BeginTextureMode (target))
       (cls)
       (color 7)
       (EndTextureMode))
        
     ; main game loop
     (λ ()
       (let loop ()
         (sync)

         ; run main game loop
         (BeginTextureMode (target))
         ((game-loop))
         (EndTextureMode)
 
         ; perform a small garbage collect
         (collect-garbage 'minor)

         ; run until the game quits
         (unless (should-quit)
           (loop))))

     ; clean-up
     (λ ()
       (stop-music)
       (StopSoundMulti)
       (CloseWindow)))))
