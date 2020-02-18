#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/vector)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(define riff-header
  (u8vector #x52 #x49 #x46 #x46    ; "RIFF"
            #x24 #x00 #x00 #x00    ; size of WAVE = size of RIFF - 8
            #x57 #x41 #x56 #x45    ; "WAVE"
            #x66 #x6d #x74 #x20    ; "fmt "
            #x10 #x00 #x00 #x00    ; 16
            #x01 #x00 #x01 #x00    ; PCM + 1 channel
            #x11 #x2b #x00 #x00    ; sample rate
            #x22 #x56 #x00 #x00    ; sample rate * channels * bits / 8
            #x02 #x00 #x10 #x00    ; 2 bytes per sample, 16 bits per sample
            #x64 #x61 #x74 #x61    ; "data"
            #x00 #x00 #x00 #x00))  ; size

;; ----------------------------------------------------

(define riff-header-size (u8vector-length riff-header))

;; ----------------------------------------------------

(define (u8vector-set-u32! vec offset u32)
  (for ([n (range 4)])
    (let ([byte (bitwise-bit-field u32 (* n 8) (* (+ n 1) 8))])
      (u8vector-set! vec (+ offset n) byte))))

;; ----------------------------------------------------

(define (wave-length duration sample-rate channels bytes-per-sample)
  (inexact->exact (ceiling (* duration sample-rate channels bytes-per-sample))))

;; ----------------------------------------------------

(define (make-riff u8-samples sample-rate channels bytes-per-sample)
  (let* ([data-size (u8vector-length u8-samples)]
         [riff-size (+ riff-header-size data-size)]
         [riff (make-u8vector riff-size)])

    ; copy the default riff header
    (for ([i (in-naturals)]
          [byte riff-header])
      (u8vector-set! riff i byte))

    ; set the number of channels and samples per second
    (u8vector-set! riff 22 channels)
    (u8vector-set-u32! riff 24 sample-rate)

    ; set the number of bytes per second
    (u8vector-set-u32! riff 28 (* sample-rate channels bytes-per-sample))

    ; set the byte rate and bits per sample
    (u8vector-set! riff 32 bytes-per-sample)
    (u8vector-set! riff 34 (* bytes-per-sample 8))

    ; set the size of the data
    (u8vector-set-u32! riff 40 data-size)

    ; copy the samples
    (for ([byte u8-samples]
          [i (in-naturals riff-header-size)])
      (u8vector-set! riff i byte))

    ; final riff
    riff))

;; ----------------------------------------------------

(define (riff? vec)
  (and (u8vector? vec)
       (> (u8vector-length vec) 4)

       ; check header
       (= (u8vector-ref vec 0) #x52)    ; R
       (= (u8vector-ref vec 1) #x49)    ; I
       (= (u8vector-ref vec 2) #x46)    ; F
       (= (u8vector-ref vec 3) #x46)))  ; F
