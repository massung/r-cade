#lang racket

(require r-cade)

;; ----------------------------------------------------

(define song (music "CCGGAAG-FFEEDDC-GGFFEED-GGFFEED-CCGGAAG-FFEEDDC-." #:tempo 120))

;; ----------------------------------------------------

(define draw-stars (action btn-z #t))

;; ----------------------------------------------------

(define (start)
  (play-music song))

;; ----------------------------------------------------

(define (game-loop)
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
      (draw (random 128)
            (random 128)
            (if (positive? (random 10))
                '(#x80)
                '(#b0001000
                  #b0000000
                  #b0001000
                  #b1011101
                  #b0001000
                  #b0001000
                  #b0000000
                  #b0001000)))))

  ; quit when escape is pressed
  (when (btn-quit)
    (quit)))

;; ----------------------------------------------------

(define (play)
  (run game-loop 128 128 #:init start #:fps 20))

;; ----------------------------------------------------

(module+ main
  (play))
