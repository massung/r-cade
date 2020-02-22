#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)
(require ffi/vector)
(require csfml)

;; ----------------------------------------------------

(require "voice.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define sample-rate 4000)
(define half-peak 32000)
(define channels 1)
(define bytes-per-sample 2)

;; ----------------------------------------------------

(define (sound curve duration [voice basic-voice])
  (let* ([count (inexact->exact (ceiling (* duration sample-rate)))]
         [samples (make-s16vector count)]
         [instrument (voice-instrument voice)]
         [envelope (voice-envelope voice)])

    ; fill sample buffer
    (for ([n (range count)])
      (let* ([time (/ n sample-rate)]
             [u (/ n count)]

             ; frequency and volume of sample
             [freq (curve u)]
             [volume (envelope u)]

             ; amplitude of wave
             [amp (instrument (* time freq pi 2))]

             ; calculate the sample
             [sample (* half-peak volume amp)])
        (s16vector-set! samples n (inexact->exact (floor sample)))))

    ; create a sound buffer from the samples
    (let ([ptr (s16vector->cpointer samples)])
      (sfSoundBuffer_createFromSamples ptr count 1 sample-rate))))

;; ----------------------------------------------------

(define (tone freq seconds [voice basic-voice])
  (sound (const freq) seconds voice))

;; ----------------------------------------------------

(define (sweep start-hz end-hz seconds [voice basic-voice])
  (let ([curve (λ (u)
                 (+ start-hz (* (- end-hz start-hz) u)))])
    (sound curve seconds voice)))

