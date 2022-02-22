#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/cvector)

;; ----------------------------------------------------

(require raylib)

;; ----------------------------------------------------

(require "voice.rkt")
(require "riff.rkt")
(require "sound.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define sounds (make-parameter #f))

;; ----------------------------------------------------

(define music-channel (make-parameter #f))
(define music-riff (make-parameter #f))

;; ----------------------------------------------------

(define sound-queue (make-parameter #f))

;; ----------------------------------------------------

(define (create-sound-channels n)
  (for/list ([_ (range n)])
    (sfSound_create)))

;; ----------------------------------------------------

(define (with-sound thunk)
  (let* ([avail (λ (sound)
                  (eq? (sfSound_getStatus sound) 'sfStopped))]

         ; find all free sound channels
         [free-sounds (filter avail (sounds))])
    (unless (empty? free-sounds)
      (thunk (first free-sounds)))))

;; ----------------------------------------------------

(define (play-queued-sounds)
  (for ([buf (remove-duplicates (sound-queue) eq?)])
    (with-sound (λ (sound)
                  (sfSound_setBuffer sound buf)
                  (sfSound_play sound)))

    ; empty queue
    (sound-queue null)))

;; ----------------------------------------------------

(define (play-sound buffer)
  (sound-queue (cons buffer (sound-queue))))

;; ----------------------------------------------------

(define (stop-sound)
  (for-each StopSound (sounds))

  ; clear the sound queue so new sounds don't play
  (sound-queue null))

;; ----------------------------------------------------

(define (sound-volume vol)
  (for-each (λ (sound) (SetSoundVolume sound vol)) (sounds)))

;; ----------------------------------------------------

(define (music-volume vol)
  (let ([chan (music-channel)])
    (when chan
      (SetMusicVolume chan vol))))

;; ----------------------------------------------------

(define (play-music riff #:loop [loop #t])
  (stop-music)

  ; ensure the music to play is a riff
  (when (riff? riff)
    (let* ([ptr (riff-ptr riff)]
           [len (riff-length riff)]
           [chan (sfMusic_createFromMemory ptr len)])
      (sfMusic_setLoop chan loop)
      (PlayMusicStream chan)
      
      ; set the new riff and channel
      (music-riff riff)
      (music-channel chan))))
  
;; ----------------------------------------------------

(define (pause-music [pause #t])
  (let ([chan (music-channel)])
    (when chan
      ((if pause PauseMusicStream ResumeMusicStream) chan))))

;; ----------------------------------------------------

(define (stop-music)
  (let ([chan (music-channel)])
    (when chan
      (StopMusicStreams chan))))
