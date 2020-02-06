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
(require "shader.rkt")
(require "input.rkt")
(require "draw.rkt")
(require "audio.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define (quit)
  (sfRenderWindow_close (window)))

;; ----------------------------------------------------

(define framerate (make-parameter #f))
(define frame (make-parameter #f))
(define frameclock (make-parameter #f))

;; ----------------------------------------------------

(define frametime (make-parameter #f))
(define gametime (make-parameter #f))

;; ----------------------------------------------------

(define-syntax define-action
  (syntax-rules ()
    [(_ name btn)
     (define (name) (btn))]
    [(_ name btn #t)
     (define (name) (eq? (btn) 1))]
    [(_ name btn rep)
     (define (name)
       (let ([rate (floor (/ (framerate) rep))])
         (and (btn) (= (remainder (btn) rate) 1))))]))

;; ----------------------------------------------------

(define (process-events)
  (update-buttons)

  ; handle every event in the queue
  (do ([event (sfRenderWindow_pollEvent (window))
              (sfRenderWindow_pollEvent (window))])
    ((not event))
    (case (sfEvent-type event)
      ; window events
      ('sfEvtClosed (sfRenderWindow_close (window)))
      ('sfEvtResized (resize (sfEvent-size event)))
      
      ; key events
      ('sfEvtKeyPressed
       (on-key-pressed (sfEvent-key event)))
      ('sfEvtKeyReleased
       (on-key-released (sfEvent-key event)))
      
      ; mouse events
      ('sfEvtMouseMoved
       (on-mouse-moved (sfEvent-mouseMove event)))
      ('sfEvtMouseButtonPressed
       (on-mouse-clicked (sfEvent-mouseButton event)))
      ('sfEvtMouseButtonReleased
       (on-mouse-released (sfEvent-mouseButton event))))))

;; ----------------------------------------------------

(define (sync)
  (process-events)
  (flip (frame) (gametime))
  
  ; wait for the next frame
  (let* ([elapsed (sfClock_getElapsedTime (frameclock))]
         [delta (- (/ (framerate)) (sfTime_asSeconds elapsed))])
    (unless (< delta 0.0)
      (sleep delta)))

  ; update frametime, gametime, frame, and reset the frame clock
  (frametime (sfTime_asSeconds (sfClock_getElapsedTime (frameclock))))
  (gametime (+ (gametime) (frametime)))
  (frame (+ (frame) 1))
  (sfClock_restart (frameclock)))

;; ----------------------------------------------------

(define (wait [until btn-any])
  (do () [(or (until) (not (sfRenderWindow_isOpen (window)))) #f]
    (sync)))

;; ----------------------------------------------------

(define (run game-loop
             pixels-wide
             pixels-high
             #:init [init #f]
             #:scale [scale 3]
             #:fps [fps 60]
             #:shader [effect #t]
             #:title [title "R-cade"])
  (let ([mode (make-sfVideoMode (* pixels-wide scale) (* pixels-high scale) 32)])

    ; initialize global state
    (parameterize
        ([window (sfRenderWindow_create mode title '(sfDefaultStyle) #f)]

         ; video memory
         [texture (sfRenderTexture_create pixels-wide pixels-high #f)]
         [sprite (sfSprite_create)]

         ; create a fragment shader for fullscreen rendering
         [shader (and effect (sfShader_createFromMemory #f #f crt-fragment-shader))]

         ; default render state
         [render-state (make-sfRenderStates sfBlendAlpha
                                            sfTransform_Identity
                                            #f
                                            #f)]

         ; playfield size
         [width pixels-wide]
         [height pixels-high]

         ; input buttons
         [buttons (make-hash)]

         ; mouse position
         [mouse-x 0]
         [mouse-y 0]

         ; frame
         [framerate fps]
         [frame 0]

         ; delta frame time, total game time, and framerate clock
         [frametime 0.0]
         [gametime 0.0]
         [frameclock (sfClock_create)])
      (cls)
      (color 7)

      ; optionally allow for an init function
      (when init
        (init))

      ; set shader uniforms
      (when (shader)
        (let ([size (make-sfGlslVec2 (exact->inexact (width))
                                     (exact->inexact (height)))])
          (sfShader_setVec2Uniform (shader) "textureSize" size)))

      ; main game loop
      (do () [(not (sfRenderWindow_isOpen (window)))]
        (with-handlers ([exn? (λ (e) (displayln e))])
          (sync)
          (game-loop)))

      ; stop playing any music
      (stop-music)
      
      ;; free memory
      (when (shader)
        (sfShader_destroy (shader)))
      (sfClock_destroy (frameclock))
      (sfSprite_destroy (sprite))
      (sfRenderTexture_destroy (texture))
      (sfRenderWindow_close (window))
      (sfRenderWindow_destroy (window)))))
