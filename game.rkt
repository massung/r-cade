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

(define goto (make-parameter cls))

;; ----------------------------------------------------

(define (sync)
  (should-quit (WindowShouldClose))

  ; all per-frame processing
  (update-camera)
  (update-frame)
  (update-buttons)
  (update-mouse-pos)
  (update-music)

  ; stop rendering to vram
  (EndTextureMode)

  ; copy vram to display
  (flip (frame) (gametime))

  ; begin rendering to vram
  (BeginTextureMode (target)))

;; ----------------------------------------------------

(define (wait [until btn-any])
  (do () [(or (until) (WindowShouldClose)) #f]
    (sync)))

;; ----------------------------------------------------

(define (calc-window-size w h [max-dim 600])
  (let ([min-dim (quotient 600 (quotient (max w h) (min w h)))])
    (if (> w h)
        (values 600 min-dim)
        (values min-dim 600))))

;; ----------------------------------------------------

(define (run initial-game-loop
             pixels-wide
             pixels-high
             #:init [init #f]
             #:fps [fps 60]
             #:shader [effect #t]
             #:title [title "R-cade"]
             #:trace-log [log-level 'LOG_NONE])
  (let-values ([(w h) (calc-window-size pixels-wide pixels-high)])
    (InitWindow w h title)
    (SetWindowState 'FLAG_WINDOW_RESIZABLE)
    (SetTargetFPS fps)
    (SetTraceLogLevel log-level))

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

       ; button map
       [buttons (make-hasheq)]

       ; mouse position
       [mouse-x 0]
       [mouse-y 0]

       ; frame
       [frame 0]

       ; no playing music
       [playing-stream #f]
       
       ; initial game state loop
       [goto initial-game-loop]
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
       (color 7))
        
     ; main game loop
     (λ ()
       (let loop ()
         (sync)

         ; run main game loop
         ((goto))
 
         ; perform a small garbage collect
         (collect-garbage 'minor)

         ; run until the game quits
         (unless (should-quit)
           (loop))))

     ; clean-up
     (λ ()
       (stop-music)
       (StopSoundMulti)
       (EnableCursor)
       (CloseWindow)))))
