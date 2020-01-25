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

(define sample-rate (make-parameter 11025))

;; ----------------------------------------------------

(define riff-header
  (u8vector #x52 #x49 #x46 #x46    ; "RIFF"
            #x24 #x00 #x00 #x00    ; size of WAVE = size of RIFF - 8
            #x57 #x41 #x56 #x45    ; "WAVE"
            #x66 #x6d #x74 #x20    ; "fmt "
            #x10 #x00 #x00 #x00    ; 16
            #x01 #x00 #x01 #x00    ; PCM + 1 channel
            #x11 #x2b #x00 #x00    ; sample rate
            #x11 #x2b #x00 #x00    ; sample rate * channels * bits / 8
            #x01 #x00 #x08 #x00    ; 1 byte per sample, 8 bits per sample
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

(define (wave-length seconds)
  (inexact->exact (round (* seconds (sample-rate)))))

;; ----------------------------------------------------

(define (make-riff num-samples)
  (let* ([riff-size (+ riff-header-size num-samples)]
         [riff (make-u8vector riff-size)])

    ; copy the rif header into the riff
    (for ([i (in-naturals)]
          [byte riff-header])
      (u8vector-set! riff i byte))

    ; set the sizes and rates
    (u8vector-set-u32! riff 4 (- riff-size 8))
    (u8vector-set-u32! riff 40 num-samples)
    (u8vector-set-u32! riff 24 (sample-rate))
    (u8vector-set-u32! riff 28 (sample-rate))

    ; final result
    riff))

;; ----------------------------------------------------

(define (write-riff riff offset instrument envelope freq num-samples)
  (for ([n (range num-samples)])
    (let* ([amp (/ (* (+ offset n) freq (* pi 2)) (sample-rate))]
           
           ; get the sample from the instrument, optionally envelope it
           [sample (let ([volume (envelope (/ n num-samples))])
                     (* volume (instrument amp)))]
           
           ; convert to a byte
           [byte (inexact->exact (floor (+ 128 (* sample 127.0))))])
      (u8vector-set! riff (+ riff-header-size offset n) byte))))
