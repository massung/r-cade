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

(require "time.rkt")
(require "video.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define buttons (make-parameter #f))

;; ----------------------------------------------------

(define mouse-x (make-parameter #f))
(define mouse-y (make-parameter #f))

;; ----------------------------------------------------

(define (update-mouse-pos)
  (let ([pos (GetScreenToWorld2D (GetMousePosition) camera)])
    (mouse-x (Vector2-x pos))
    (mouse-y (Vector2-y pos))))

;; ----------------------------------------------------

(define (update-button btn pred)
  (let ([update! (λ (time)
                   (and (pred btn) (+ (frametime) (or time 0))))])
    (hash-update! (buttons) btn update! (λ () #f))))

;; ----------------------------------------------------

(define (update-buttons)
  (for ([btn '(KEY_Z
               KEY_X
               KEY_UP
               KEY_DOWN
               KEY_RIGHT
               KEY_LEFT
               KEY_TAB
               KEY_SPACE
               KEY_ENTER)])
    (update-button btn IsKeyDown))

  ; mouse button
  (update-button 'MOUSE_BUTTON_LEFT IsMouseButtonDown))

;; ----------------------------------------------------

(define (btn? btn)
  (hash-ref (buttons) btn #f))

;; ----------------------------------------------------

(define (btn-z) (btn? 'KEY_Z))
(define (btn-x) (btn? 'KEY_X))
(define (btn-up) (btn? 'KEY_UP))
(define (btn-down) (btn? 'KEY_DOWN))
(define (btn-right) (btn? 'KEY_RIGHT))
(define (btn-left) (btn? 'KEY_LEFT))
(define (btn-start) (btn? 'KEY_ENTER))
(define (btn-select) (btn? 'KEY_TAB))
(define (btn-jump) (btn? 'KEY_SPACE))
(define (btn-mouse) (btn? 'MOUSE_BUTTON_LEFT))

;; ----------------------------------------------------

(define (btn-any)
  (or (btn-start)
      (btn-select)
      (btn-jump)
      (btn-z)
      (btn-x)
      (btn-mouse)))

;; ----------------------------------------------------

(define (action pred [hold #f] [rate #f])
  (let ([hits 0])
    (λ ()
      (let* ([time (pred)]
             [hit? (and time (cond
                               [(zero? hits) #t]

                               ; no repeats allowed
                               [(not hold) #f]

                               ; no rate limit
                               [(not rate) #t]

                               ; enough time elapsed for another hit
                               [else (> time (/ hits rate))]))])

        ; increment hit count or reset hit
        (begin0 hit?
                (cond
                  [hit? (set! hits (add1 hits))]

                  ; button was released, reset hits
                  [(not time) (set! hits 0)]))))))
