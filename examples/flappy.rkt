#lang racket

(require r-cade)

;; ----------------------------------------------------

(define rider #((#x06 #x37 #x26 #x32 #x3f #x32 #x7a #xfe #xfc #x78 #x70 #x60)
                (#x06 #xb7 #xe6 #xf2 #xff #xf2 #x7a #xfe #xfc #x78 #x10)))

;; ----------------------------------------------------

(define flap-sound (sweep 100 200 0.1 #:instrument square-wave #:envelope z-envelope))
(define die-sound (sweep 200 100 0.4  #:instrument sawtooth-wave #:envelope z-envelope))

;; ----------------------------------------------------

(define player-x 20)
(define player-y 64)
(define player-frame 0)
(define player-vel 0)

;; ----------------------------------------------------

(define score 0)

;; ----------------------------------------------------

(define game-started #f)
(define game-over #f)
(define gravity 0.0)
(define terminal-vel 120.0)
(define ground-scroll 0)
(define pipe-spawn-counter 0)

;; ----------------------------------------------------

(struct pipe [(x #:mutable) top gap])

;; ----------------------------------------------------

(define pipes null)

;; ----------------------------------------------------

(define-action flap btn-z #t)

;; ----------------------------------------------------

(define (spawn-pipe)
  (when (zero? (remainder pipe-spawn-counter 54))
    (let* ([x   (+ (width) 8)]
           [gap (+ (random 32) 32)]
           [top (+ (random (- 90 gap)) 20)])
      (set! pipes (cons (pipe x top gap) pipes))))
  (set! pipe-spawn-counter (+ pipe-spawn-counter 1)))

;; ----------------------------------------------------

(define (draw-pipes)
  (for ([pipe pipes])
    (let ([x   (pipe-x pipe)]
          [top (pipe-top pipe)]
          [gap (pipe-gap pipe)])
      (for ([i (range top)])
        (color 11)
        (draw x i '(#xff))
        (color 7)
        (draw x i '(#x40))
        (color 3)
        (draw x i '(#x01)))
      (for ([i (range (+ top gap) 118)])
        (color 11)
        (draw x i '(#xff))
        (color 7)
        (draw x i '(#x40))
        (color 3)
        (draw x i '(#x01)))
      (color 0)
      (draw x top '(#xff))
      (draw x (+ top gap) '(#xff)))))

;; ----------------------------------------------------

(define (move-pipes)
  (for ([pipe pipes])
    (set-pipe-x! pipe (- (pipe-x pipe) 1))

    ; increase if the player got through a pipe
    (when (= player-x (+ (pipe-x pipe) 8))
      (set! score (+ score 1)))
  
  ; get rid of any pipes off screen
  (set! pipes (filter (λ (p) (> (pipe-x p) -8)) pipes))))

;; ----------------------------------------------------

(define (draw-player)
  (color 2)
  (draw player-x player-y (vector-ref rider player-frame)))

;; ----------------------------------------------------

(define (animate-player)
  (when (zero? (remainder (frame) 5))
    (set! player-frame (bitwise-xor player-frame 1))))

;; ----------------------------------------------------

(define (control-player)
  (when (flap)
    (unless game-started
      (set! game-started #t)
      (set! gravity 8.0)
      (set! pipe-spawn-counter 0))

    ; boost the player
    (set! player-vel -100.0)
    (play-sound flap-sound)))

;; ----------------------------------------------------

(define (move-player)
  (set! player-vel (min (+ player-vel gravity) terminal-vel))
  (set! player-y (+ player-y (* (frametime) player-vel))))

;; ----------------------------------------------------

(define (dead?)
  (when game-started
    (unless game-over
      (set! game-over (or (> player-y 118)
                          (for/or ([pipe pipes])
                            (let ([x   (pipe-x pipe)]
                                  [top (pipe-top pipe)]
                                  [gap (pipe-gap pipe)])
                              (and (< (- x 8) player-x (+ x 8))
                                   (or (< player-y top)
                                       (< (+ top gap) (+ player-y 6))))))))

      ; play death sound if game over
      (when game-over
        (play-sound die-sound)))))

;; ----------------------------------------------------

(define (draw-ground)
  (color 15)
  (rect 0 120 (width) 8 #:fill #t)

  ; draw the grass
  (for ([i (range 17)])
    (color 11)
    (draw (+ ground-scroll (* i 8)) 118 '(#x44 #x88))
    (color 3)
    (draw (+ ground-scroll (* i 8) 1) 118 '(#x3b #x77)))

  ; scroll the ground
  (unless game-over
    (set! ground-scroll (remainder (- ground-scroll 1) 8))))

;; ----------------------------------------------------

(define (draw-score)
  (let ([s (cond
              [game-over "Game Over! START to Try Again"]
              [game-started (format "SCORE: ~a" score)]
              [else "Press Z to flap..."])])
    (color 0)
    (text 1 2 s)
    (color 7)
    (text 1 1 s)))

;; ----------------------------------------------------

(define (setup)
  (set! game-started #f)
  (set! game-over #f)                       
  (set! player-y 64)
  (set! player-vel 0)
  (set! gravity 0)
  (set! score 0)
  (set! pipes null))

;; ----------------------------------------------------

(define (new-game)
  (setup)
  
  (λ ()
    (cls 12)

    ; player
    (unless game-over
      (animate-player)
      (control-player))
    (move-player)

    ; pipes
    (when game-started
      (unless game-over
        (spawn-pipe)
        (move-pipes)))

    ; world
    (draw-player)
    (draw-ground)
    (draw-pipes)
    (draw-score)

    ; check for game over
    (dead?)

    ; restart?
    (when (btn-start)
      (setup))

    ; quit?
    (when (btn-quit)
      (quit))))

;; ----------------------------------------------------

(define (play)
  (run (new-game) 128 128 #:fps 30 #:title "R-cade: Flappy"))
