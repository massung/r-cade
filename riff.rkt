#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

(struct riff [ptr length])

;; ----------------------------------------------------

(define riff-header
  (vector #x52 #x49 #x46 #x46    ; "RIFF"
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

(define riff-header-size (vector-length riff-header))

;; ----------------------------------------------------

(define (ptr-set-u32! bytes offset u32)
  (for ([n (range 4)])
    (let ([byte (bitwise-bit-field u32 (* n 8) (* (+ n 1) 8))])
      (ptr-set! bytes _byte (+ offset n) byte))))

;; ----------------------------------------------------

(define (wave-length duration sample-rate channels bytes-per-sample)
  (inexact->exact (ceiling (* duration sample-rate channels bytes-per-sample))))

;; ----------------------------------------------------

(define (make-riff u8-samples sample-rate channels bytes-per-sample)
  (let* ([data-size (vector-length u8-samples)]
         [riff-size (+ riff-header-size data-size)]
         [bytes (malloc _byte riff-size 'atomic-interior)])

    ; copy the default riff header
    (for ([i (in-naturals)]
          [byte riff-header])
      (ptr-set! bytes _byte i byte))

    ; set the number of channels and samples per second
    (ptr-set! bytes _byte 22 channels)
    (ptr-set-u32! bytes 24 sample-rate)

    ; set the number of bytes per second
    (ptr-set-u32! bytes 28 (* sample-rate channels bytes-per-sample))

    ; set the byte rate and bits per sample
    (ptr-set! bytes _byte 32 bytes-per-sample)
    (ptr-set! bytes _byte 34 (* bytes-per-sample 8))

    ; set the size of the data
    (ptr-set-u32! bytes 40 data-size)

    ; copy the samples
    (for ([byte u8-samples]
          [i (in-naturals riff-header-size)])
      (ptr-set! bytes _byte i byte))

    ; final riff
    (riff bytes riff-size)))

;; ----------------------------------------------------

(define (save-riff riff filename)
  (let ([write-riff (λ (port)
                      (for ([i (riff-length riff)])
                        (write-byte (ptr-ref (riff-ptr riff) _byte i) port)))])
    (call-with-output-file filename write-riff #:exists 'replace)))
