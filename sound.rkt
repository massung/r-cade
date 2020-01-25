#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/vector)
(require csfml)

;; ----------------------------------------------------

(require "voice.rkt")
(require "riff.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(struct sound [riff buffer])

;; ----------------------------------------------------

(define (waveform curve seconds #:instrument [inst sin] #:envelope [env (const 1.0)])
  (let* ([length (wave-length seconds)]
         [riff (make-riff length)])

    ; write all the samples to the riff
    (for ([n (range length)])
      (write-riff riff n inst env (curve (/ n length)) 1))

    ; create the sound buffer object
    (let ([pointer (u8vector->cpointer riff)]
          [length (u8vector-length riff)])
      (sound riff (sfSoundBuffer_createFromMemory pointer length)))))

;; ----------------------------------------------------

(define (tone freq seconds #:instrument [inst sin] #:envelope [env (const 1.0)])
  (waveform (const freq) seconds #:instrument inst #:envelope env))

;; ----------------------------------------------------

(define (sweep start end seconds #:instrument [inst sin] #:envelope [env (const 1.0)])
  (let ([curve (λ (u)
                 (+ start (* (- end start) u)))])
    (waveform curve seconds #:instrument inst #:envelope env)))
