#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)
(require ffi/unsafe/custodian)
(require racket/match)

;; ----------------------------------------------------

(require "video.rkt")
(require "shader.rkt")
(require "input.rkt")
(require "font.rkt")
(require "draw.rkt")
(require "palette.rkt")
(require "audio.rkt")
(require "sound.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define (quit)
  (when (and (window) (sfRenderWindow_isOpen (window)))
    (sfRenderWindow_close (window))))

;; ----------------------------------------------------

(define framerate (make-parameter #f))
(define frame (make-parameter #f))
(define frameclock (make-parameter #f))

;; ----------------------------------------------------

(define frametime (make-parameter #f))
(define gametime (make-parameter #f))

;; ----------------------------------------------------

(define game-loop (make-parameter (λ ()
                                    (cls)
                                    (when (btn-quit)
                                      (quit)))))

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

(define-syntax define-timer
  (syntax-rules ()
    [(_ name time)
     (define name
       (let ([timer time])
         (λ ([reset #f] #:dilation [m 1])
           (match reset
             [#t (set! timer time)]

             ; normal countdown
             [#f (let ([expired (<= timer 0)])
                   (begin0 expired
                           (set! timer (if expired
                                           time
                                           (- time (* (frametime) m))))))]

             ; change the timer and reset it
             [nt (set! time nt)
                 (set! timer time)]))))]))

;; ----------------------------------------------------

(define (process-events)
  (update-buttons)

  ; handle every event in the queue
  (do ([event (sfRenderWindow_pollEvent (window))
              (sfRenderWindow_pollEvent (window))])
    ((not event))

    ; every once in a while SFML returns an invalid event
    (with-handlers ([exn:fail? (const (void))])
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
         (on-mouse-released (sfEvent-mouseButton event)))))))

;; ----------------------------------------------------

(define (goto update)
  (game-loop update))
  
;; ----------------------------------------------------

(define (sync)
  (process-events)
  (play-queued-sounds)

  ; render vram
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

(define (discover-good-scale w h)
  (let* ([mode (sfVideoMode_getDesktopMode)]

         ; size of the display the window will be on
         [screen-w (sfVideoMode-width mode)]
         [screen-h (sfVideoMode-height mode)]

         ; scale to fit ~60% of the screen
         [max-w (inexact->exact (floor (* screen-w 0.6)))]
         [max-h (inexact->exact (floor (* screen-h 0.6)))]

         ; scale to fit ~60% of the screen
         [scale-x (quotient max-w w)]
         [scale-y (quotient max-h h)])
    (inexact->exact (max (min scale-x scale-y) 1))))

;; ----------------------------------------------------

(define (handle-exn exn)
  (displayln exn)
  ((error-display-handler) "ERROR" exn)
  (quit))

;; ----------------------------------------------------

(define (run initial-game-loop
             pixels-wide
             pixels-high
             #:init [init #f]
             #:scale [scale #f]
             #:fps [fps 60]
             #:shader [effect #t]
             #:title [title "R-cade"])
  (unless scale
    (set! scale (discover-good-scale pixels-wide pixels-high)))

  ; set the video mode for the window
  (let ([mode (make-sfVideoMode (* pixels-wide scale) (* pixels-high scale) 32)])

    ; initialize global state
    (parameterize
        ([window (sfRenderWindow_create mode title '(sfDefaultStyle) #f)]

         ; video memory
         [texture (sfRenderTexture_create pixels-wide pixels-high #f)]
         [sprite (sfSprite_create)]

         ; create a fragment shader for fullscreen rendering
         [shader (and effect (sfShader_createFromMemory vertex-shader
                                                        #f
                                                        fragment-shader))]

         ; default render state
         [render-state (make-sfRenderStates sfBlendAlpha
                                            sfTransform_Identity
                                            #f
                                            #f)]

         ; default palette
         [palette (for/vector ([c basic-palette]) c)]

         ; default font
         [font basic-font]

         ; sound mixer
         [sounds (create-sound-channels 8)]
         [sound-queue null]

         ; music channel and riff pointer
         [music-channel #f]
         [music-riff #f]

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
         [frameclock (sfClock_create)]

         ; initial game state loop
         [game-loop initial-game-loop])

      ; attempt to close the windw on shutdown (or re-run)
      (let ([v (register-custodian-shutdown (window)
                                            (λ (w)
                                              (when w
                                                (sfRenderWindow_close w)))
                                            #:at-exit? #t)])
        (dynamic-wind

         ; initialization
         (λ ()
           (when init
             (init))

           ; defaults
           (cls)
           (color 7)

           ; set shader uniforms
           (when (shader)
             (let ([size (make-sfGlslVec2 (exact->inexact (width))
                                          (exact->inexact (height)))])
               (sfShader_setVec2Uniform (shader) "textureSize" size))))
        
         ; main game loop
         (λ ()
           (do () [(not (sfRenderWindow_isOpen (window)))]
             (sync)

             ; execute the game loop
             ((game-loop))))

         ; clean-up
         (λ ()
           (unregister-custodian-shutdown (window) v)

           ; stop playing sounds and music
           (stop-music)
           (stop-sound)))))))
