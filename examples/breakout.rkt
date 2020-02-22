#lang racket

(require r-cade)

;; ----------------------------------------------------

(define paddle-x 57)
(define paddle-y 121)

;; ----------------------------------------------------

(define ball-x 0)
(define ball-y 0)

;; ----------------------------------------------------

(define dx 0)
(define dy 0)

;; ----------------------------------------------------

(define ddx 0)
(define ddy 0)

;; ----------------------------------------------------

(define lives 5)
(define score 0)
(define combo 1)

;; ----------------------------------------------------

(define blocks (make-vector 80 #t))

;; ----------------------------------------------------

(define scrolling-bg #((#x44 #x00 #x00 #x00)
                       (#x00 #x44 #x00 #x00)
                       (#x00 #x00 #x44 #x00)
                       (#x00 #x00 #x00 #x44)
                       (#x22 #x00 #x00 #x00)
                       (#x00 #x22 #x00 #x00)
                       (#x00 #x00 #x22 #x00)
                       (#x00 #x00 #x00 #x22)))

;; ----------------------------------------------------

(define bounce (sweep 440 300 0.1 (voice sawtooth-wave z-envelope)))

;; ----------------------------------------------------

(define boop (tone 200 0.15 (voice square-wave fade-out-envelope)))

;; ----------------------------------------------------

(define die (sweep 200 100 0.8 (voice sawtooth-wave fade-out-envelope)))

;; ----------------------------------------------------

(define (setup)
  (set! lives (- lives 1))
  (set! combo 1)

  ; reset ball position
  (set! ball-x (+ paddle-x 7))
  (set! ball-y (- paddle-y 2))

  ; reset ball vector
  (set! dy -1)
  (set! dx (- (* (random) 3.0) 1.5)))

;; ----------------------------------------------------

(define (draw-background)
  (color 1)
  (for ([x (range 0 127 8)])
    (for ([y (range 0 127 4)])
      (let* ([n (vector-length scrolling-bg)]
             [i (remainder (+ (quotient (frame) 4) y) n)])
        (draw x y (vector-ref scrolling-bg i))))))

;; ----------------------------------------------------

(define (draw-paddle)
  (color 3)
  (for ([i (range 2)])
    (draw (+ paddle-x (* i 8)) paddle-y '(#xff #xff)))
  
  ; draw pips
  (color 15)
  (let ([y (+ paddle-y 4)])
    (cond
      [(= lives 1) (draw (+ paddle-x 8) y '(#x80))]
      [(= lives 2) (draw (+ paddle-x 7) y '(#xa0))]
      [(= lives 3) (draw (+ paddle-x 6) y '(#xa8))]
      [(= lives 4) (draw (+ paddle-x 5) y '(#xaa))])))

;; ----------------------------------------------------

(define (draw-blocks #:x-offset [x-offset 16] #:y-offset [y-offset 18])
  (let ([cx (+ ball-x 1)]
        [cy (+ ball-y 1)])
    (for ([x (range 10)])
      (for ([y (range 8)])
        (let* ([i (+ (* y 10) x)]

               ; block bounds
               [bx1 (+ (* x 10) x-offset)]
               [bx2 (+ bx1 7)]
               [by1 (+ (* y 3) y-offset)]
               [by2 (+ by1 1)])

          ; is this block still alive?
          (when (vector-ref blocks i)
            (color (+ 8 y))
            (draw bx1 by1 '(#xff #xff))

            ; Pretend the ball is a circle with a slightly larger radius
            ; than 1 pixel to determine collision.

            (let* ([cx (+ ball-x dx 0.5)]
                   [cy (+ ball-y dy 0.5)]

                   ; find the nearest point on the block to the ball
                   [nx (max bx1 (min cx bx2))]
                   [ny (max by1 (min cy by2))]

                   ; calculate distance from the block to the ball
                   [dist-x (* (- nx cx) (- nx cx))]
                   [dist-y (* (- ny cy) (- ny cy))])

              ; is the distance within the radius of the ball?
              (when (< (+ dist-x dist-y) 2.25)  ; 1.5 ^ 2
                (if (< dist-y (/ dist-x 2))
                    (set! ddx (- dx))
                    (set! ddy (- dy)))

                ; clear the block
                (vector-set! blocks i #f)

                ; increment combo counter and tally score
                (set! score (+ score (* combo 10)))
                (set! combo (min (+ combo 1) 8))

                ; play the boop sound
                (play-sound boop)))))))))
  
;; ----------------------------------------------------

(define (draw-score)
  (color 7)
  (text 1 1 (format "Score: ~a" score))
  (text 92 1 (format "Combo: *~a" combo)))
  
;; ----------------------------------------------------

(define-action move-left btn-left)
(define-action move-right btn-right)
(define-action reset-ball btn-start #t)
  
;; ----------------------------------------------------

(define (breakout)
  (cls)
    
  ; move paddle left/right
  (when (and (move-left) (> paddle-x 0))
    (set! paddle-x (- paddle-x 2)))
  (when (and (move-right) (< paddle-x 112))
    (set! paddle-x (+ paddle-x 2)))

  ; background tiles and paddle
  (draw-background)
  (draw-paddle)

  ; draw the ball
  (color 7)
  (draw ball-x ball-y '(#xc0 #xc0))

  ; advance the ball
  (set! ball-x (+ ball-x dx))
  (set! ball-y (+ ball-y dy))
  (set! ddx dx)
  (set! ddy dy)

  ; clamp the ball to the walls
  (set! ball-x (max (min ball-x 126) 0))

  ; draw the blocks and score
  (let ([y (round (+ 24 (* (sin (/ (frame) 60)) 10)))])
    (draw-blocks #:y-offset y)
    (draw-score))

  ; bounce the ball off walls
  (when (or (= ball-x 0) (= ball-x 126))
    (play-sound bounce)
    (set! ddx (- dx)))
  (when (= ball-y 0)
    (play-sound bounce)
    (set! ddy (- dy)))

  ; fall off board
  (when (and (> ball-y (height)) (> ddy 0))
    (play-sound die)
    (set! ddx 0)
    (set! ddy 0))

  ; bounce off the paddle
  (when (= ball-y (- paddle-y 1))
    (let ([x (- ball-x paddle-x)])
      (when (<= -1 x 15)
        (play-sound bounce)
        (set! ddy (- dy))
        (set! combo 1)
        (cond
          ([<= 0 x 5] (set! ddx (max (- dx (random)) -2.0)))
          ([<= 10 x 15] (set! ddx (min (+ dx (random)) 2.0)))))))

  ; change ball direction?
  (set! dx ddx)
  (set! dy ddy)
  
  ; reset?
  (when (and (reset-ball) (> lives 0))
    (setup))
  
  ; should the demo end?
  (when (btn-quit)
    (quit)))
  
;; ----------------------------------------------------

(define (play)
  (setup)
  (run breakout 128 128 #:title "R-cade: Breakout"))
