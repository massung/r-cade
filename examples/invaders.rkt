#lang racket

(require r-cade)
(require racket/random)

;; ----------------------------------------------------

(define invader-sprites
  #(#((#x3c #x7e #x5a #xff #xa5 #x99 #x5a #x5a #x42)
      (#x3c #x7e #x5a #xff #xa5 #x99 #x5a #x5a #x42))
    #((#x18 #x5a #xbd #x99 #x99 #xa5 #x24 #x42 #x24)
      (#x18 #x5a #xbd #x99 #x99 #xa5 #x24 #x42 #x24))
    #((#x3c #x7e #xb7 #x7e #x3c #x24 #x42 #x5a #x24)
      (#x3c #x7e #xeb #x7e #x3c #x24 #x42 #x5a #x24))
    #((#x3c #x7e #xcf #xcf #xff #x3a #x22 #x22 #xee)
      (#x3c #x7e #xf3 #xf3 #xff #x5c #x44 #x44 #x77))
    #((#x18 #x7e #xdb #xff #x42 #x42 #x22 #x11 #x09)
      (#x18 #x7e #xdb #xff #x42 #x42 #x44 #x88 #x90))))

;; ----------------------------------------------------

(define spaceship-sprite '(#x38 #x7c #xaa #xfe #x7c #x38))

;; ----------------------------------------------------

(define player-sprite '(#x10 #x38 #x7c #x38 #x38 #xfe #x7c #xfe #xfe))

;; ----------------------------------------------------

(define barrier-sprites '((#x00 #x7f #xff #xff #xff #xff #xff #xc0)
                          (#x00 #xfe #xff #xff #xff #xff #xff #x03)))

;; ----------------------------------------------------

(struct invader [(x #:mutable) (y #:mutable) sprite])
(struct missile [(x #:mutable) (y #:mutable) velocity sprite])
(struct barrier [x (sprite #:mutable)])

;; ----------------------------------------------------

(define level 1)
(define score 0)

;; ----------------------------------------------------

(define player-x 80)
(define player-y 118)
(define player-missile #f)

;; ----------------------------------------------------

(define invaders null)
(define invader-missiles null)
(define invader-velocity 10.0)
(define invader-x-offset 4)
(define invader-y-offset 10)
(define invader-x-padding 16)
(define invader-y-padding 12)
(define invader-y-step 5)

;; ----------------------------------------------------

(define spaceship-timer 10.0)
(define spaceship #f)

;; ----------------------------------------------------

(define barriers null)
(define barrier-y 100)

;; ----------------------------------------------------

(define (spawn-invaders)
  (set! invaders (for/list ([i (range 30)])
                   (let-values ([(gy gx) (quotient/remainder i 6)])
                     (let ([x (+ (* gx invader-x-padding) invader-x-offset)]
                           [y (+ (* gy invader-y-padding) invader-y-offset)])
                       (invader x y gy)))))

  ; initial velocity
  (set! invader-velocity 10.0))

;; ----------------------------------------------------

(define (draw-invaders)
  (color 4)
  (let ([anim-frame (if (< invader-velocity 0) 0 1)])
    (for ([i invaders])
      (let ([anim (vector-ref invader-sprites (invader-sprite i))])
        (draw (invader-x i) (invader-y i) (vector-ref anim anim-frame))))))

;; ----------------------------------------------------

(define (draw-invader-missiles)
  (color 9)
  (for ([m invader-missiles])
    (draw (missile-x m) (missile-y m) (missile-sprite m))))

;; ----------------------------------------------------

(define (draw-spaceship)
  (when spaceship
    (color 13)
    (draw spaceship 10 spaceship-sprite)))

;; ----------------------------------------------------

(define (advance-invaders)
  (let ([dx (* invader-velocity (frametime))])
    (when (for/fold ([drop #f])
                    ([i invaders])
            (set-invader-x! i (+ (invader-x i) dx))

            ; TODO: shoot?
            
            ; should all invaders drop?
            (or drop (if (< invader-velocity 0)
                         (< (invader-x i) invader-x-offset)
                         (> (invader-x i) (- (width) 8 invader-x-offset)))))

      ; drop invaders
      (for ([i invaders])
        (set-invader-y! i (+ (invader-y i) invader-y-step)))

      ; flip (and increase) velocity
      (set! invader-velocity (* invader-velocity -1.1)))))

;; ----------------------------------------------------

(define (spawn-spaceship)
  (unless spaceship
    (set! spaceship-timer (- spaceship-timer (frametime)))

    ; spawn when timer expires
    (when (< spaceship-timer 0.0)
      (set! spaceship (width))
      (set! spaceship-timer (+ (random 10) 10.0)))))

;; ----------------------------------------------------

(define (advance-spaceship)
  (when spaceship
    (set! spaceship (- spaceship (* 40 (frametime))))

    ; TODO: drop bombs

    ; done?
    (when (< spaceship -8)
      (set! spaceship #f))))

;; ----------------------------------------------------

(define (advance-missile m)
  (let ([y (missile-y m)]
        [v (missile-velocity m)])
    (set-missile-y! m (+ y (* v (frametime))))

    ; on-screen?
    (< -4 y (height))))

;; ----------------------------------------------------

(define (advance-player-missile)
  (when player-missile
    (unless (advance-missile player-missile)
      (set! player-missile #f))))

;; ----------------------------------------------------

(define (advance-invader-missiles)
  (set! invader-missiles (filter advance-missile invader-missiles)))

;; ----------------------------------------------------

(define (kill-invaders)
  (when player-missile
    (let ([x (missile-x player-missile)]
          [y (missile-y player-missile)])
      (set! invaders (for/fold ([unhit-invaders null])
                               ([i invaders])
                       (let ([ix (invader-x i)]
                             [iy (invader-y i)])
                         (if (and (<= ix x (+ ix 8))
                                  (<= iy y (+ iy 8)))
                             (begin
                               (set! player-missile #f)
                               (set! score (+ score 50))

                               ; don't keep this invader
                               unhit-invaders)
                             (cons i unhit-invaders))))))))

;; ----------------------------------------------------

(define (kill-player)
  (or (ormap (λ (i) (> (+ (invader-y i) 8) barrier-y)) invaders)
      (ormap (λ (m)
               (and (<= (- player-x 4) (missile-x m) (+ player-x 3))
                    (<= (- player-y 1) (missile-y m) (+ player-y 8))))
             invader-missiles)))

;; ----------------------------------------------------

(define (spawn-barriers)
  (let* ([x0 (/ (width) 4)]
         [x1 (/ (width) 2)]
         [x2 (+ x0 x1)])
    (set! barriers (list (barrier (- x0 8) (first barrier-sprites))
                         (barrier x0 (second barrier-sprites))
                         (barrier (- x1 8) (first barrier-sprites))
                         (barrier x1 (second barrier-sprites))
                         (barrier (- x2 8) (first barrier-sprites))
                         (barrier x2 (second barrier-sprites))))))

;; ----------------------------------------------------

(define (launch-missile)
  (set! player-missile (missile player-x (- player-y 4) -80.0 '(#x80 #x80 #x80))))

;; ----------------------------------------------------

(define (drop-missile)
  (unless (null? invaders)
    (let* ([i (random-ref invaders)]
           
           ; where to spawn the missile
           [x (invader-x i)]
           [y (invader-y i)]
           
           ; spawn the missile
           [m (missile (+ x 4) (+ y 8) 40.0 '(#x80 #x80))])
      (set! invader-missiles (cons m invader-missiles)))))

;; ----------------------------------------------------

(define (destroy-barriers)
  (for ([m invader-missiles])
    (let ([mx (inexact->exact (round (missile-x m)))]
          [my (inexact->exact (round (missile-y m)))])

      ; is the missle within the y-boundary of barriers?
      (when (<= barrier-y my (+ barrier-y 7))
        (for ([b barriers])
          (let ([bx (barrier-x b)])

            ; is the missile within the x-boundary of the barrier?
            (when (<= bx mx (+ bx 7))
              (let ([barrier-bits (barrier-sprite b)])
                (for ([missile-bits (missile-sprite m)]

                      ; scanline y-offset from the top of barrier
                      [y (range (- my barrier-y) 7)])
              
                  ; does the missile sprite bitmask intersect the barrier?
                  (let ([bbits (list-ref barrier-bits y)]
                        [mbits (arithmetic-shift missile-bits (- bx mx))])
                    (unless (zero? (bitwise-and mbits bbits))
                      (let ([n (bitwise-and bbits (bitwise-not mbits))])
                        (set-barrier-sprite! b (list-set barrier-bits y n)))

                      ; move the missile off-screen
                      (set-missile-y! m (height)))))))))))))

;; ----------------------------------------------------

(define (draw-player)
  (color 11)
  (draw (- player-x 3) player-y player-sprite)

  ; draw the player's missile
  (when player-missile
    (color 7)
    (draw (missile-x player-missile)
          (missile-y player-missile)
          (missile-sprite player-missile))))

;; ----------------------------------------------------

(define (draw-barriers)
  (color 8)
  (for ([b barriers])
    (draw (barrier-x b) barrier-y (barrier-sprite b))))

;; ----------------------------------------------------

(define (draw-ground)
  (color 3)
  (let ([y (+ player-y 9)])
    (rect 0 y (width) (- (height) y) #:fill #t)))

;; ----------------------------------------------------

(define (draw-score)
  (color 6)
  (text 1 1 (format "Score: ~a" score))
  (text (- (width) 32) 1 (format "Level: ~a" level)))

;; ----------------------------------------------------

(define (next-level)
  (set! player-x (/ (width) 2))
  (set! player-y (- (height) 12))
  
  (set! level (+ level 1))
  
  (set! invader-missiles null)

  (spawn-invaders))

;; ----------------------------------------------------

(define (new-game)
  (set! level 10)
  (set! score 0)

  ; create the barriers
  (spawn-barriers)

  ; advance to the next level
  (next-level))

;; ----------------------------------------------------

(define (game-loop)
  (cls)

  ; draw
  (draw-ground)
  (draw-barriers)
  (draw-invaders)
  (draw-invader-missiles)
  (draw-spaceship)
  (draw-player)
  (draw-score)

  ; spawn the spaceship?
  (spawn-spaceship)

  ; advance
  (advance-invaders)
  (advance-spaceship)
  (advance-player-missile)
  (advance-invader-missiles)

  ; collision detection
  (kill-invaders)
  (destroy-barriers)

  ; is the player dead?
  (when (kill-player)
    (void))

  ; shoot if no missile
  (unless player-missile
    (launch-missile))

  ; drop invader missile
  (when (< (length invader-missiles) level)
    (drop-missile))

  ; player controls
  (when (and (> player-x 3) (btn-left))
    (set! player-x (- player-x (* 40 (frametime)))))
  (when (and (< player-x (- (width) 4)) (btn-right))
    (set! player-x (+ player-x (* 40 (frametime)))))

  (when (btn-quit)
    (quit)))

;; ----------------------------------------------------

(run game-loop 160 128 #:init new-game #:title "R-cade: Invaders")
