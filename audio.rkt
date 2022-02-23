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
(require "music.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define playing-stream (make-parameter #f))

;; ----------------------------------------------------

(define (play-sound sound)
  (PlaySoundMulti (sound%-sample sound)))

;; ----------------------------------------------------

(define stop-sound StopSoundMulti)

;; ----------------------------------------------------

(define (sound-volume sound vol)
  (SetSoundVolume (sound%-sample sound) vol))

;; ----------------------------------------------------

(define music-volume SetMusicVolume)

;; ----------------------------------------------------

(define (play-music music #:loop [loop #t])
  (let ([stream (music%-stream music)])

    ; stop any currently playing music stream
    (when (playing-stream)
      (StopMusicStream (playing-stream)))

    ; set looping flag
    (set-Music-looping! stream loop)

    ; start this music stream
    (playing-stream stream)
    (PlayMusicStream stream)))
  
;; ----------------------------------------------------

(define (update-music)
  (when (playing-stream)
    (UpdateMusicStream (playing-stream))))
  
;; ----------------------------------------------------

(define pause-music PauseMusicStream)
(define resume-music ResumeMusicStream)

;; ----------------------------------------------------

(define stop-music StopMusicStream)
