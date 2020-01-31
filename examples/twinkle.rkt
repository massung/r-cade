#lang racket

(require r-cade)

;; ----------------------------------------------------

(define song (music "CCGGAAG-FFEEDDC-GGFFEED-GGFFEED-CCGGAAG-FFEEDDC-." #:bpm 120))

;; ----------------------------------------------------

(define-action draw-stars btn-z)

;; ----------------------------------------------------

(define (my-game)
  (play-music song)

  ; main game loop
  (lambda ()
    (cls)
    (color (frame))

    ; animate text
    (for ([c "Twinkle Twinkle Little Star!"]
          [i (in-naturals)])
      (color (+ (frame) i))
      (let ([y (+ 11 (* 6 (sin (+ (frame) i))))])
        (text (+ 10 (* i 4)) y c)))

    ; some random sprites
    (when (draw-stars)
      (for ([i (range 5)])
        (color (random 7 16))
        (draw (random 128) (random 128) '(#b0001000
                                          #b0000000
                                          #b0001000
                                          #b1011101
                                          #b0001000
                                          #b0001000
                                          #b0000000
                                          #b0001000))))

    ; quit when escape is pressed
    (when (btn-quit)
      (quit))))

;; ----------------------------------------------------

(define (play)
  (run (my-game) 128 128 #:fps 20))
