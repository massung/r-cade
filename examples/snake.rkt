#lang racket

(require (prefix-in r: r-cade))

;; ----------------------------------------------------

(define snake-cx #f)
(define snake-cy #f)
(define snake-angle #f)
(define snake-w #f)
(define snake-body #f)
(define snake-length #f)
(define snake-segment-length #f)
(define snake-health #f)
(define snake-score #f)

;; ----------------------------------------------------

(define hi-score 400)

;; ----------------------------------------------------

(define snake-radius 15.0)
(define snake-size 2.0)

;; ----------------------------------------------------

(define food #f)
(define food-size 1.0)

;; ----------------------------------------------------

(define yum (r:sweep 300 100 0.1 (r:voice r:square-wave r:basic-envelope)))

;; ----------------------------------------------------

(define (wrap-x x)
  (let ([w (r:width)])
    (cond
      [(< x 0) (+ x w)]
      [(> x w) (- x w)]
      [else x])))

;; ----------------------------------------------------

(define (wrap-y y)
  (let ([h (r:height)])
    (cond
      [(< y 0) (+ y h)]
      [(> y h) (- y h)]
      [else y])))

;; ----------------------------------------------------

(define (snake-x)
  (wrap-x (+ snake-cx (* (cos snake-angle) snake-radius))))

;; ----------------------------------------------------

(define (snake-y)
  (wrap-y (+ snake-cy (* (sin snake-angle) snake-radius))))

;; ----------------------------------------------------

(define (snake-head)
  (list (snake-x) (snake-y)))

;; ----------------------------------------------------

(define (draw-snake-head [color 11])
  (r:color color)
  (r:circle (snake-x) (snake-y) (+ snake-size 1.0) #:fill #t))

;; ----------------------------------------------------

(define (snake-segments)
  (let ([n (* snake-segment-length 2)])
    (if (< (length snake-body) n)
        null
        (in-slice n (drop snake-body n)))))

;; ----------------------------------------------------

(define (draw-snake-body [color 3])
  (r:color color)
  (for ([seg (snake-segments)])
    (match seg
      [(list x y rest ...)
       (r:circle x y snake-size #:fill #t)])))

;; ----------------------------------------------------

(define (draw-snake [dead #f])
  (draw-snake-body (if dead 8 3))
  (draw-snake-head (if dead 14 11)))

;; ----------------------------------------------------

(define (shift-body)
  (set! snake-body (append (snake-head)
                           (if (< (length snake-body) snake-length)
                               snake-body
                               (drop-right snake-body 2)))))

;; ----------------------------------------------------

(define (advance-head)
  (set! snake-angle (+ snake-angle (* snake-w (r:frametime)))))

;; ----------------------------------------------------

(define (turn-snake)
  (let ([dx (- (snake-x) snake-cx)]
        [dy (- (snake-y) snake-cy)])
    (set! snake-w (- snake-w))

    ; shift the center of the snake's pivot
    (set! snake-cx (wrap-x (+ snake-cx dx dx)))
    (set! snake-cy (wrap-y (+ snake-cy dy dy)))

    ; invert the angle to the opposite side of the circle
    (set! snake-angle (+ snake-angle pi))))

;; ----------------------------------------------------

(define (grow-snake [points 0])
  (set! snake-score (+ snake-score points))
  (set! snake-length (+ snake-length (* snake-segment-length 8))))

;; ----------------------------------------------------

(define (random-pos)
  (list (random (r:width))
        (random (r:height))))

;; ----------------------------------------------------

(define (spawn-food [n 1])
  (let ([new-food (for/list ([i (range n)])
                    (random-pos))])
    (set! food (append food new-food))))

;; ----------------------------------------------------

(define (draw-food)
  (r:color 14)
  (for ([pos food])
    (match pos
      [(list x y)
       (r:draw (- x 1) (- y 1) '(#x40 #xe0 #x40))])))

;; ----------------------------------------------------

(define (increase-health)
  (set! snake-health (min 60 (+ snake-health 2))))

;; ----------------------------------------------------

(define (eat-food)
  (let ([sx (snake-x)]
        [sy (snake-y)]

        ; r^2 to compare against for circle-circle overlap
        [sz (* (+ snake-size food-size)
               (+ snake-size food-size))])

    ; split the food into 2 lists: overlapping and not
    (let-values ([(eaten not-eaten)
                  (partition (λ (pos)
                               (match pos
                                 [(list fx fy)
                                  (let ([dx (- fx sx)]
                                        [dy (- fy sy)])
                                    (< (+ (* dx dx) (* dy dy)) sz))]))
                             food)])

      ; grow the snake and spawn new food to replace it
      (let ([new-food (for/list ([_ eaten])
                        (increase-health)
                        (grow-snake 50)
                        (r:play-sound yum)
                        (random-pos))])
        (set! food (append not-eaten new-food))))))

;; ----------------------------------------------------

(define (update-snake)
  (shift-body)
  (advance-head)
  (eat-food)

  ; lose health
  (set! snake-health (- snake-health (* 2 (r:frametime))))

  ; handle player input
  (when (or (and (positive? snake-w) (r:btn-left))
            (and (negative? snake-w) (r:btn-right)))
    (turn-snake)))

;; ----------------------------------------------------

(define (draw-health)
  (r:color (cond
             [(< snake-health 10) 8]
             [(< snake-health 30) 10]
             [else 11]))

  ; draw the health box
  (let ([x (- (/ (r:width) 2)snake-health)]
        [y (- (r:height) 2)])
    (r:rect x y (* snake-health 2) 2 #:fill #t)))

;; ----------------------------------------------------

(define (draw-score)
  (r:color 7)
  (r:text 2 2 snake-score)
  (r:color 10)
  (r:text (/ (r:width) 2) 2 hi-score))

;; ----------------------------------------------------

(define (test-collision)
  (let ([x (snake-x)]
        [y (snake-y)]
        [r (* snake-size snake-size)])
    (for/or ([seg (snake-segments)])
      (match seg
        [(list sx sy rest ...)
         (let ([dx (- sx x)]
               [dy (- sy y)])
           (< (+ (* dx dx) (* dy dy)) r))]))))

;; ----------------------------------------------------

(define (game-over)
  (r:cls)

  ; draw the dead snake in the background
  (draw-score)
  (draw-snake #t)

  ; draw game over stats
  (let ([x (- (/ (r:width) 2) 20)]
        [y (- (/ (r:height) 2) 20)])
    (r:color 7)
    (r:text x y "GAME OVER")
    (r:color 11)
    (r:text x (+ y 10) "Press START"))

  (when (r:btn-start)
    (new-game)
    (r:goto game-loop)))

;; ----------------------------------------------------

(define (game-loop)
  (r:cls)

  ; per frame processing
  (if (or (test-collision) (negative? snake-health))
      (r:goto game-over)
      (begin
        (update-snake)

        ; update the high score
        (set! hi-score (max hi-score snake-score))

        ; draw everything
        (draw-health)
        (draw-food)
        (draw-snake)
        (draw-score)))

  ; quit?
  (when (r:btn-quit)
    (r:quit)))

;; ----------------------------------------------------

(define (new-game)
  (set! snake-cx (/ (r:width) 2))
  (set! snake-cy (/ (r:height) 2))
  (set! snake-angle 0.0)
  (set! snake-w pi)
  (set! snake-health 60)
  (set! snake-score 0)
  (set! snake-body null)
  (set! snake-length 0)
  (set! snake-segment-length 5)

  ; grow the snake a few segments
  (grow-snake)

  ; spawn the initial food batch
  (set! food (for/list ([i (range 10)])
               (random-pos))))

;; ----------------------------------------------------

(define (play)
  (r:run game-loop 120 120 #:init new-game #:title "R-cade: Snake"))

;; ----------------------------------------------------

(module+ main
  (play))
