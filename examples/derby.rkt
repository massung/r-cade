#lang racket

(require r-cade)

;; ----------------------------------------------------

(define horse-sprite
  #((#x001c #x003e #x007e #x7ff0 #x8fe0 #x0fe0 #x1ff8 #x2836 #xc809 #x1008 #x2000)
    (#x000c #x001e #x003f #x3ff2 #xcfe0 #x0fe0 #x1fe0 #x1820 #x2850 #xc890 #x1110)
    (#x0006 #x000f #x001f #x1ff8 #xeff0 #x0fe0 #x1fe0 #x2860 #x1850 #x0de0 #x0840)
    (#x000c #x001e #x003f #x9ff2 #x6fe0 #x0fe0 #x0ff0 #x1c30 #x1418 #x22e4 #x2100)))

;; ----------------------------------------------------

(define jockey-sprite '(#x06 #x06 #x7c #xf6 #xf0 #x38 #x08 #x10 #x38))

;; ----------------------------------------------------

(define rail-sprite
  #((#xff #xff #x18 #x18 #x18 #x18 #x18)
    (#xff #xff)
    (#xff #xff)
    (#xff #xff)))

;; ----------------------------------------------------

(define ribbon-sprite '(#x3c #x7e #xff #xff #xff #xff #x7e #x3c #x18 #x3c #x3c #x66 #x66 #x66 #x66))

;; ----------------------------------------------------

(define call-to-post (music "A3---D4---F---A---A-A--F---F-F---D---F---D---A3---------------"
                            #:tempo 900
                            #:voice (voice (synth (sin 1)
                                                  (triangle-wave 0.2)
                                                  (noise-wave 0.15))
                                           (envelope 0 1 0.7 0.7 0.2 0.8 0.2 0.8 0))))

;; ----------------------------------------------------

(define clop-sound (tone 50 0.05 (voice square-wave (envelope 0 0.2 0.15 0))))

;; ----------------------------------------------------

(define positions #("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th"))

;; ----------------------------------------------------

(struct horse [color
               jockey
               lane
               aggro
               min-stamina
               (time #:mutable)
               (stamina #:mutable)
               (recover #:mutable)
               (frame #:mutable)
               (x #:mutable)
               (vel #:mutable)
               (fps #:mutable)
               (position #:mutable)])

;; ----------------------------------------------------

(define player #f)
(define recover-timer 0)
(define horses null)
(define start-x -16)
(define start-vel 16)
(define start-fps 10)
(define finish-line 640)
(define score 0)
(define ribbons null)

;; ----------------------------------------------------

(define action-btn btn-z)

;; ----------------------------------------------------

(define (track-offset)
  (max (inexact->exact (floor (- (* (horse-x player) 4) 32))) 0))

;; ----------------------------------------------------

(define (railing-offset)
  (- (remainder (track-offset) 32)))

;; ----------------------------------------------------

(define (draw-railing)
  (for ([segment (range (+ (quotient (width) 32) 2))])
    (for ([sprite rail-sprite]
          [offset (in-naturals)])
      (let ([x (+ (railing-offset) (* segment 32) (* offset 8))])
        (color (if (odd? offset) 8 7))
        (draw x 19 sprite)
        (draw x 103 sprite)))))

;; ----------------------------------------------------

(define (draw-finish-line)
  (let ([x (+ (- (* finish-line 4) (track-offset)) 4)])
    (color 8)
    (rect x 26 0 77)))

;; ----------------------------------------------------

(define (draw-track)
  (color 4)
  (rect 0 0 (width) 8 #:fill #t)
  (color 15)
  (rect 0 7 (width) 9 #:fill #t)

  ; back grass
  (color 3)
  (rect 0 16 (width) 4 #:fill #t)

  ; railing lane
  (color 11)
  (rect 0 20 (width) 8 #:fill #t)

  ; racing lanes
  (for ([lane (range 9)])
    (let ([y (+ 28 (* lane 8))])
      (color (if (odd? lane) 11 3))
      (rect 0 y (width) 8 #:fill #t)))

  ; railing lane
  (color 11)
  (rect 0 100 (width) 8 #:fill #t)

  ; front grass
  (color 3)
  (rect 0 108 (width) 4 #:fill #t))

;; ----------------------------------------------------

(define (horse-anim-frame h)
  (bitwise-and (inexact->exact (floor (horse-frame h))) 3))

;; ----------------------------------------------------

(define (draw-horse h)
  (let* ([frame (horse-anim-frame h)]
         [y (+ 22 (* (horse-lane h) 8))]
         [x (cond
              [(< (horse-x h) 32) (horse-x h)]
              [(eq? h player) 32]
              [else (+ 32 (- (horse-x h) (horse-x player)))])])
    (color (horse-color h))
    (draw-ex x y (vector-ref horse-sprite frame))
    (color (horse-jockey h))
    (draw (+ x 5) (- y 3) jockey-sprite)))

;; ----------------------------------------------------

(define (draw-horses)
  (for ([h horses])
    (draw-horse h))
  (draw-horse player))

;; ----------------------------------------------------

(define (draw-stamina)
  (let ([stamina (horse-stamina player)])
    (color 7)
    (text 1 1 "Stamina:")
    (rect 33 1 stamina 5 #:fill #t)
    (color 8)
    (rect (+ stamina 33) 1 (- 100 stamina) 5 #:fill #t)))

;; ----------------------------------------------------

(define (draw-place)
  (color 7)
  (text (- (width) 12) 1 (vector-ref positions (horse-position player))))

;; ----------------------------------------------------

(define (use-crop h)
  (let ([ds (/ (horse-vel h) 5)])
    (set-horse-stamina! h (max (- (horse-stamina h) ds) 0))
    (set-horse-recover! h 0.25))  ; (/ (- 100 (horse-stamina h)) 50)))

  ; reset timer
  (set-horse-time! h 0.0)
  
  ; increase velocity of player's horse
  (let ([m (/ (min (horse-stamina h) 50) 50)])
    (set-horse-vel! h (+ (horse-vel h) (* (frametime) 25 m)))
    (set-horse-fps! h (+ (horse-fps h) (* (frametime) 20 m)))))

;; ----------------------------------------------------

(define (recover-stamina h)
  (if (> (horse-recover h) 0)
      (set-horse-recover! h (max (- (horse-recover h) (frametime)) 0.0))
      (set-horse-stamina! h (min (+ (horse-stamina h) (* (frametime) 50)) 100))))

;; ----------------------------------------------------

(define (place-horses)
  (let ([order (sort (cons player horses) > #:key horse-x)])
    (for ([h (cons player horses)])
      (unless (> (+ (horse-x h) 16) finish-line)
        (set-horse-position! h (index-of order h eq?))))))

;; ----------------------------------------------------

(define (advance-horse h)
  (set-horse-x! h (+ (horse-x h) (* (horse-vel h) (frametime))))
  (set-horse-frame! h (+ (horse-frame h) (* (horse-fps h) (frametime))))
  (set-horse-time! h (+ (horse-time h) (frametime)))
  
  ; slow down horse
  (let ([decay (* (frametime) (if (eq? h player) 0.7 (random)))])
    (set-horse-fps! h (max (- (horse-fps h) decay) 5))
    (set-horse-vel! h (max (- (horse-vel h) decay) 16))))

;; ----------------------------------------------------

(define (advance-horses)
  (for ([h horses])
    (advance-horse h)

    ; randomly use the crop
    (when (> (horse-x h) 32)
      (let ([r (horse-recover h)]
            [m (horse-min-stamina h)]
            [a (horse-aggro h)])
        (if (or (and (> r 0.0) (> (horse-stamina h) m) (> (horse-time h) a))
                (and (= r 0.0) (> (horse-stamina h) (/ (- 100 m) 2))))
            (use-crop h)
            (recover-stamina h)))))
  (advance-horse player))

;; ----------------------------------------------------

(define (spawn-horse color jockey lane)
  (let* ([level (+ (length ribbons) 1)]
         [aggro (* 1/60 (+ 4 (* (random) 3)))]
         [min-sta (apply min (for/list ([_ (range level)])
                               (* (random) 100)))])
    (horse color jockey lane aggro min-sta 0.0 100 0.0 0 start-x start-vel start-fps 0)))

;; ----------------------------------------------------

(define (next-race)
  (cls)
  (play-music call-to-post #:loop #f)

  ; ready the next level...
  (set! player (spawn-horse 9 10 8))
  (set! horses (for/list ([lane (range 8)]
                          [color (list 1 4 6 8 10 12 13 14)]
                          [jockey (list 12 9 1 14 0 5 7 2)])
                 (spawn-horse color jockey lane))))

;; ----------------------------------------------------

(define (ribbon-color pos)
  (vector-ref #(12 8 10 7 14 11) pos))

;; ----------------------------------------------------

(define (award-ribbon)
  (cls)

  ; draw past ribbons at bottom
  (for ([r ribbons] [n (in-naturals)])
    (let-values ([(y x) (quotient/remainder n 14)])
      (color (ribbon-color r))
      (draw (+ 6 (* x 12)) (+ 48 (* y 18)) ribbon-sprite)))

  ; draw the ribbon and award $$
  (let ([pos (horse-position player)])
    (if (< pos 6)
        (let ([winnings (vector-ref #(1000 500 400 250 100 50) pos)])
          (color (ribbon-color pos))
          (draw (- (/ (width) 2) 4) 10 ribbon-sprite)

          ; award $$ based on position
          (set! score (+ score winnings))
          (set! ribbons (cons pos ribbons))

          ; show current winnings
          (color 7)
          (text 50 30 (format "You won $~a !!" winnings))
          (text 50 38 (format "Total winnings: $~a" score))
          (text 40 104 "Press START for next race...")

          ; wait to advance to the next race
          (wait btn-start)
          (next-race))

        ; game over
        (begin
          (color 7)
          (text 6 10 "GAME OVER")
          (text 6 18 "Better luck next time!")
          (text 6 34 (format "Final winnings: $~a" score))
          (text 71 104 "My ribbons")
          (wait)
          (quit)))))

;; ----------------------------------------------------

(define (game-loop)
  (draw-track)
  (draw-railing)
  (draw-finish-line)
  (draw-horses)
  (draw-stamina)
  (draw-place)

  ; advance horses
  (advance-horses)
  (place-horses)

  ; hoof sounds
  (when (= (horse-anim-frame player) 1)
    (play-sound clop-sound))

  ; player controls
  (when (> finish-line (horse-x player) 32)
    (if (eq? (action-btn) 1)
        (begin
          (use-crop player)
          (set! action-btn (if (eq? action-btn btn-z) btn-x btn-z)))
        (recover-stamina player)))

  ; end of race?
  (when (> (horse-x player) (+ finish-line 100))
    (award-ribbon))

  ; quit game?
  (when (btn-quit)
    (quit)))

;; ----------------------------------------------------

(run game-loop 180 112 #:init next-race #:scale 3 #:title "R-cade: Derby")
