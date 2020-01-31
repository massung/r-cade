#lang racket

(require r-cade)

;; ----------------------------------------------------

(define snake-x #f)
(define snake-y #f)
(define snake-body #f)
(define snake-length #f)
(define snake-growth #f)
(define snake-angle #f)
(define snake-angular-vel #f)
(define snake-food #f)
(define snake-health #f)
(define snake-score #f)

;; ----------------------------------------------------

(define yum (sweep 300 100 0.1 #:instrument square-wave #:envelope z-envelope))

;; ----------------------------------------------------

(define (grow-snake [n 10])
  (set! snake-growth (+ snake-growth n)))

;; ----------------------------------------------------

(define (random-food)
  (list (random 127)
        (random 124)))

;; ----------------------------------------------------

(define (eat-food)
  (set! snake-food (for/list ([food snake-food])
                     (let ([x (first food)]
                           [y (second food)])
                       (if (and (<= (- snake-x 1) (+ x 1))
                                (>= (+ snake-x 1) x)
                                (<= (- snake-y 1) (+ y 1))
                                (>= (+ snake-y 1) y))
                           (begin
                             (play-sound yum)
                             (grow-snake)

                             ; add points and health
                             (set! snake-score (+ snake-score 1000))
                             (set! snake-health (min (+ snake-health 5) 60))

                             ; a new piece of food
                             (random-food))

                           ; not eaten, keep food
                           food)))))

;; ----------------------------------------------------

(define (advance-snake)
  (let ([dx (cos snake-angle)]
        [dy (sin snake-angle)])
    (set! snake-health (- snake-health (frametime)))
    (eat-food)

    ; append the head to the body
    (let ([head (list snake-x snake-y)])
      (set! snake-body (cons head snake-body)))

    ; slither (or grow) the body
    (if (> snake-growth 0)
        (begin
          (set! snake-growth (- snake-growth 1))
          (set! snake-length (+ snake-length 1)))
        (set! snake-body (drop-right snake-body 1)))

    ; tally points
    (when (zero? (remainder (frame) 10))
      (set! snake-score (+ snake-score 1)))

    ; update the snake's position
    (set! snake-x (+ snake-x dx))
    (set! snake-y (+ snake-y dy))

    ; wrap playfield
    (when (< snake-x 0) (set! snake-x 127))
    (when (< snake-y 0) (set! snake-y 122))
    (when (>= snake-x 128) (set! snake-x 0))
    (when (>= snake-y 123) (set! snake-y 0))

    ; apply angular velocity
    (let ([da (* snake-angular-vel (frametime))])
      (set! snake-angle (+ snake-angle da)))))

;; ----------------------------------------------------

(define (draw-snake)
  (color 3)
  (for ([pos snake-body])
    (let ([x (first pos)]
          [y (second pos)])
      (draw x y '(#x80))))

  ; head is bigger
  (color 11)
  (draw (- snake-x 1) (- snake-y 1) '(#xe0 #xe0 #xe0)))

;; ----------------------------------------------------

(define (draw-food)
  (color 8)
  (for ([food snake-food])
    (let ([x (first food)]
          [y (second food)])
      (draw x y '(#xc0 #xc0)))))

;; ----------------------------------------------------

(define (draw-score)
  (color 7)
  (text 2 2 (format "Score: ~a" snake-score))

  (when (> snake-health 0)
    (color (cond
             [(< snake-health 10) 8]
             [(< snake-health 20) 9]
             [(< snake-health 30) 10]
             [else 11]))
    
    ; draw the health bar
    (rect 0 (- (height) 3) (/ (* snake-health (width)) 60) 3 #:fill #t)))

;; ----------------------------------------------------

(define (control-snake)
  (when (or (and (> snake-angular-vel 0) (btn-left))
            (and (< snake-angular-vel 0) (btn-right)))
    (set! snake-angular-vel (- snake-angular-vel))))

;; ----------------------------------------------------

(define (setup)
  (set! snake-growth 8)
  (set! snake-length 0)
  (set! snake-score 0)
  (set! snake-health 60)
  (set! snake-x 64)
  (set! snake-y 64)
  (set! snake-angle 0.0)
  (set! snake-angular-vel pi)
  (set! snake-body null)
  (set! snake-food (for/list ([i (range 10)])
                     (random-food))))

;; ----------------------------------------------------

(define (snake-collide)
  (let ([n (+ snake-growth 4)])
    (if (< snake-length n)
        #f
        (for/or ([body (drop snake-body n)])
          (let ([x (first body)]
                [y (second body)])
            (and (<= (- snake-x 1) x (+ snake-x 1))
                 (<= (- snake-y 1) y (+ snake-y 1))))))))

;; ----------------------------------------------------

(define (game-over?)
  (if (or (<= snake-health 0) (snake-collide))
      (begin
        (color 7)
        (text 47 50 "GAME OVER")
        (color 10)
        (text 47 58 snake-score)

        ; allow restarting the game
        (when (btn-start)
          (setup)))
      #f))

;; ----------------------------------------------------

(define (new-game)
  (setup)

  ; game loop
  (λ ()
    (cls)

    ; per frame processing
    (unless (game-over?)
      (advance-snake)
      (draw-score)
      (draw-food)
      (draw-snake)
      (control-snake))

    ; quit?
    (when (btn-quit)
      (quit))))

;; ----------------------------------------------------

(define (play)
  (run (new-game) 128 128 #:title "R-cade: Snake"))
