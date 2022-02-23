#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require raylib)

;; ----------------------------------------------------

(require "riff.rkt")
(require "voice.rkt")

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(struct sound% [wave sample])

;; ----------------------------------------------------

(define sample-rate 4000)
(define half-peak 24000)
(define channels 1)
(define bytes-per-sample 2)

;; ----------------------------------------------------

(define (sound curve duration [voice basic-voice])
  (let* ([count (inexact->exact (ceiling (* duration sample-rate)))]
         [samples (make-vector (* count bytes-per-sample))]
         [instrument (voice-instrument voice)]
         [envelope (voice-envelope voice)])

    ; fill sample buffer
    (for ([n (range 0 (vector-length samples) bytes-per-sample)])
      (let* ([time (/ n bytes-per-sample sample-rate)]
             [u (/ n bytes-per-sample count)]

             ; frequency and volume of sample
             [freq (curve u)]
             [volume (envelope u)]

             ; amplitude of wave
             [amp (instrument (* time freq pi 2))]

             ; calculate the sample
             [sample (inexact->exact (floor (* half-peak volume amp)))]

             ; split the sample into upper and lower bytes
             [lsb (bitwise-bit-field sample 0 8)]
             [msb (bitwise-bit-field sample 8 16)])
        (vector-set! samples (+ n 0) lsb)
        (vector-set! samples (+ n 1) msb)))
    
    ; create a riff from the sound buffer
    (let* ([riff (make-riff samples sample-rate channels bytes-per-sample)]
           [wave (LoadWaveFromMemory ".wav" (riff-ptr riff) (riff-length riff))])
      (sound% wave (LoadSoundFromWave wave)))))

;; ----------------------------------------------------

(define (tone freq seconds [voice basic-voice])
  (sound (const freq) seconds voice))

;; ----------------------------------------------------

(define (sweep start-hz end-hz seconds [voice basic-voice])
  (let ([curve (λ (u)
                 (+ start-hz (* (- end-hz start-hz) u)))])
    (sound curve seconds voice)))
