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
;(require "audio.rkt")
;(require "sound.rkt")
(require "time.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define (quit)
  (CloseWindow))

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

  ; render vram
  (flip (frame) (gametime)))

;; ----------------------------------------------------

(define (wait [until btn-any])
  (do () [(or (until) (WindowShouldClose)) #f]
    (sync)))

;; ----------------------------------------------------

(define (monitor-size)
  (let ([monitor (GetCurrentMonitor)])
    (values (GetMonitorWidth monitor)
            (GetMonitorHeight monitor))))

;; ----------------------------------------------------

(define (resize-window pixels-wide pixels-high scale)
  (let-values ([(screen-w screen-h) (monitor-size)])
    (let* ([w (inexact->exact (* pixels-wide scale))]
           [h (inexact->exact (* pixels-high scale))]

           ; center the window
           [x (inexact->exact (- (/ screen-w 2) (* w 0.5)))]
           [y (inexact->exact (- (/ screen-h 2) (* h 0.5)))])
      (SetWindowSize w h)
      (SetWindowPosition x y))))

;; ----------------------------------------------------

(define (run initial-game-loop
             pixels-wide
             pixels-high
             #:init [init #f]
             #:scale [scale #f]
             #:fps [fps 60]
             #:shader [effect #t]
             #:title [title "R-cade"]
             #:trace-log [log-level 'LOG_NONE])
  (InitWindow pixels-wide pixels-high title)
  (SetWindowState 'FLAG_WINDOW_RESIZABLE)
  (SetTargetFPS fps)
  (SetTraceLogLevel log-level)
  
  ; try and discover the scale
  (unless scale
    (let-values ([(w h) (monitor-size)])
      (let ([scale-x (quotient (* w 0.6) pixels-wide)]
            [scale-y (quotient (* h 0.6) pixels-high)])
        (set! scale (max (min scale-x scale-y 3) 1)))))

  ; resize the window accordingly
  (resize-window pixels-wide pixels-high scale)

  ;; create the sprite masks
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
       
       ; initial game state loop
       [game-loop initial-game-loop])

    ; attempt to close the windw on shutdown (or re-run)
    (let ([v (register-custodian-shutdown (target)
                                          (λ (w) (CloseWindow))
                                          #:at-exit? #t)])
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
           (unless (WindowShouldClose)
             (loop))))

         ; clean-up
         (λ ()
           (CloseWindow)

           ; stop playing sounds and music
           ;(StopMusicStream)
           ;(stop-sound)
           
           (unregister-custodian-shutdown (target) v))))))
