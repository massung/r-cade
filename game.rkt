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
(define frametime (make-parameter #f))

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
  (flip)
  
  ; wait for the next frame
  (let* ([next (+ (/ 1000 (framerate)) (frametime))]
         [delta (- next (current-milliseconds))])
    (unless (< delta 0.0)
      (sleep (/ delta 1000))))

  ; update frame
  (frame (+ (frame) 1))
  (frametime (current-milliseconds)))

;; ----------------------------------------------------

(define (wait [until btn-any])
  (do () [(or (until) (not (sfRenderWindow_isOpen (window)))) #f]
    (sync)))

;; ----------------------------------------------------

(define (run game-loop w h #:scale [scale 3] #:fps [fps 60] #:title [title "R-cade"])
  (let ([mode (make-sfVideoMode (* w scale) (* h scale) 32)])

    ; initialize global state
    (parameterize
        ([window (sfRenderWindow_create mode title '(sfDefaultStyle) #f)]

         ; video memory
         [texture (sfRenderTexture_create w h #f)]
         [sprite (sfSprite_create)]

         ; playfield size
         [width w]
         [height h]

         ; input buttons
         [buttons (make-hash)]

         ; mouse position
         [mouse-x 0]
         [mouse-y 0]

         ; frame
         [framerate fps]
         [frame 0]

         ; time of last frame
         [frametime (current-inexact-milliseconds)])
      (cls)
      (color 7)

      ; main game loop
      (do () [(not (sfRenderWindow_isOpen (window)))]
        (sync)
        
        (with-handlers ([exn? (λ (e) (displayln e))])
          (game-loop)))

      ; stop playing any music
      (stop-music)
      
      ;; free memory
      (sfSprite_destroy (sprite))
      (sfRenderTexture_destroy (texture))
      (sfRenderWindow_close (window))
      (sfRenderWindow_destroy (window)))))
