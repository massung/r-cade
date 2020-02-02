#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/vector)
(require csfml)

;; ----------------------------------------------------

(require "riff.rkt")
(require "voice.rkt")
(require "sound.rkt")
(require "music.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define channels
  (list (sfSound_create)
        (sfSound_create)
        (sfSound_create)
        (sfSound_create)))

;; ----------------------------------------------------

(define (with-channel thunk)
  (let* ([in-use (λ (channel)
                   (let ([status (sfSound_getStatus channel)])
                     (not (eq? status 'sfStopped))))]
         
         ; find the first, stopped voice available
         [avail-channels (dropf channels in-use)])
    (if (null? avail-channels)
        #f
        (let ([chan (first avail-channels)])
          (thunk chan)

          ; return the channel for stop, pause, etc.
          chan))))

;; ----------------------------------------------------

(define (play-sound sound #:volume [volume 100.0] #:pitch [pitch 1.0] #:loop [loop #f])
  (with-channel (λ (channel)
                  (sfSound_setBuffer channel (waveform-buffer sound))

                  ; channel settings
                  (sfSound_setVolume channel volume)
                  (sfSound_setPitch channel pitch)
                  (sfSound_setLoop channel loop)

                  ; play it
                  (sfSound_play channel))))

;; ----------------------------------------------------

(define (stop-sound channel)
  (sfSound_stop channel))

;; ----------------------------------------------------

(define sound? waveform?)
(define channel? sfSound*?)
(define music? tune?)

;; ----------------------------------------------------

(define music transcribe-notes)

;; ----------------------------------------------------

(define music-channel
  (let ([pointer (u8vector->cpointer riff-header)]
        [length (u8vector-length riff-header)])
    (sfMusic_createFromMemory pointer length)))

;; ----------------------------------------------------

(define (play-music tune #:volume [volume 100.0] #:pitch [pitch 1.0] #:loop [loop #t])
  (stop-music)

  ; set the new, active music tune
  (set! music-channel (tune-music tune))
  
  ; start playing the new music
  (sfMusic_setVolume music-channel volume)
  (sfMusic_setPitch music-channel pitch)
  (sfMusic_setLoop music-channel loop)
  (sfMusic_play music-channel))

;; ----------------------------------------------------

(define (pause-music [pause #t])
  ((if pause sfMusic_pause sfMusic_play) music-channel))

;; ----------------------------------------------------

(define (stop-music)
  (sfMusic_stop music-channel))
