#lang racket

(require r-cade)

;; ----------------------------------------------------

(define player-sprite '(#x20 #x20 #x70 #xa8 #xf8 #x70 #x70))

;; ----------------------------------------------------

(define laser-sprite '(#x80 #x80 #x80 #x80))

;; ----------------------------------------------------

(define mushroom-sprite
  #((#x00 #x30 #x78 #x00 #x30 #x30)
    (#x00 #x30 #x78)
    (#x00 #x30 #x50)))

;; ----------------------------------------------------

(define mushroom-cap-sprite
  #((#x30 #x48 #x84 #xfc)
    (#x30 #x48 #x84 #xd4)
    (#x30 #x48 #x84)))

;; ----------------------------------------------------

(define dropper-sprite '(#x30 #x78 #xfc #xfc #xfc #x48 #x24))

;; ----------------------------------------------------

(define spider-sprite
  '(#x0000 #x0000 #x0000 #x0100 #x07c0 #x0540 #x07c0 #x07c0 #x0380))

(define spider-legs-sprite
  #((#x0000 #x0000 #x701c #x8822 #x0000 #x0000 #x701c #x8822 #x0000)
    (#x0000 #x2008 #x5014 #x8822 #x0000 #x2008 #x5014 #x8822 #x0000)
    (#x1010 #x1010 #x2828 #x2828 #x4004 #x1010 #x1010 #x2828 #x4004)
    (#x0000 #x0000 #x0000 #x3838 #x4004 #x8002 #x0820 #x0820 #x3018)
    (#x1010 #x1010 #x2828 #x2828 #x4004 #x1010 #x1010 #x2828 #x4004)
    (#x0000 #x2008 #x5014 #x8822 #x0000 #x2008 #x5014 #x8822 #x0000)))

;; ----------------------------------------------------

(define up-sprite '(#x30 #x78 #xfc #xfc #xfc #x78))
(define down-sprite '(#x78 #xfc #xfc #xfc #x78 #x30))
(define right-sprite '(#x70 #xf8 #xfc #xfc #xf8 #x70))
(define left-sprite '(#x18 #x7c #xfc #xfc #x7c #x18))

;; ----------------------------------------------------

(define level-colors #(8 9 10 11 12 13 14 15))

;; ----------------------------------------------------

(define (level-color n)
  (vector-ref level-colors (modulo n (vector-length level-colors))))

;; ----------------------------------------------------

(struct body [size x y d] #:mutable)

;; ----------------------------------------------------

(define level 1)
(define score 0)
(define hi-score 5000)
(define player-x #f)
(define player-y #f)
(define laser-x -1)
(define laser-y -1)
(define mushrooms #f)
(define centipede-offset 0)
(define centipedes '())

;; ----------------------------------------------------

(define (primary-color) (level-color level))
(define (secondary-color) (level-color (- level 1)))
(define (tertiary-color) (level-color (+ level 2)))

;; ----------------------------------------------------

(define cells-wide 20)
(define cells-high 24)

;; ----------------------------------------------------

(define y-offset 12)
(define player-area 30)

;; ----------------------------------------------------

(define (laser?)
  (and (>= laser-x 0) (>= laser-y 0)))

;; ----------------------------------------------------

(define (spawn-centipede n)
  (let* ([x (random cells-wide)]
         [centipede (for/list ([i (range n)])
                      (body i x -1 'down))])
    (set! centipedes (cons centipede centipedes))))

;; ----------------------------------------------------

(define (draw-score)
  (color (primary-color))
  (font wide-font)
  (text 4 1 score)
  (text 50 1 hi-score))

;; ----------------------------------------------------

(define (draw-player)
  (color (primary-color))
  (draw player-x player-y player-sprite))

;; ----------------------------------------------------

(define (draw-laser)
  (color (tertiary-color))
  (draw laser-x laser-y laser-sprite))

;; ----------------------------------------------------

(define (draw-body body)
  (let-values ([(dx dy) (case (body-d body)
                          [(left) (values (- centipede-offset) 0)]
                          [(right) (values (+ centipede-offset) 0)]
                          [(up) (values 0 (- centipede-offset))]
                          [(down) (values 0 (+ centipede-offset))])])
    (let ([x (+ (* (body-x body) 6) dx)]
          [y (+ (* (body-y body) 6) dy y-offset)])
      (color (primary-color))
      (draw x y (case (body-d body)
                  [(left) left-sprite]
                  [(right) right-sprite]
                  [(up) up-sprite]
                  [(down) down-sprite])))))

;; ----------------------------------------------------

(define (draw-centipedes)
  (for ([centipede centipedes])
    (for ([body centipede])
      (draw-body body))))

;; ----------------------------------------------------

(define (move-centipedes)
  (set! centipede-offset (+ centipede-offset 1))
  (when (= centipede-offset 6)
    (set! centipede-offset 0)
    (for ([centipede centipedes])
      (for ([body centipede])
        (if (= (body-size body) 0)
            (case (body-d body)
              [(left) (set-body-x! body (- (body-x body) 1))]
              [(right) (set-body-x! body (+ (body-x body) 1))]
              [(up) (set-body-y! body (- (body-y body) 1))]
              [(down) (set-body-y! body (+ (body-y body) 1))])
            (set-body-size! body (- (body-size body) 1)))))))

;; ----------------------------------------------------

(define (draw-spider x y)
  (let ([legs (anim-sprite spider-legs-sprite (gametime))])
    (color (primary-color))
    (draw-ex x y spider-sprite)
    (color (tertiary-color))
    (draw-ex x y legs)))

;; ----------------------------------------------------

(define (draw-mushroom cell-x cell-y [n 0])
  (let ([sprite (vector-ref mushroom-sprite n)]
        [cap (vector-ref mushroom-cap-sprite n)]
        [x (* cell-x 6)]
        [y (+ (* cell-y 6) y-offset)])
    (color (primary-color))
    (draw x y sprite)
    (color (secondary-color))
    (draw x y cap)))

;; ----------------------------------------------------

(define (draw-mushrooms)
  (for ([m mushrooms] [i (in-naturals)] #:when m)
    (let-values ([(y x) (quotient/remainder i cells-wide)])
      (draw-mushroom x y m))))

;; ----------------------------------------------------

(define (move-laser)
  (let ([dy 300])
    (set! laser-y (- laser-y (* dy (frametime))))

    ; get rid when gone
    (when (< laser-y 0)
      (set! laser-y -1)
      (set! laser-x -1))))

;; ----------------------------------------------------

(define (shoot-mushroom)
  (let* ([cell-x (quotient laser-x 6)]
         [cell-y (quotient (- (exact-round laser-y) y-offset) 6)]
         [i (+ (* cell-y cells-wide) (exact-round cell-x))])
    (when (< -1 i (vector-length mushrooms))
      (let ([m (vector-ref mushrooms i)])
        (when m
          (let ([n (+ m 1)])
            (vector-set! mushrooms i (if (= n (vector-length mushroom-sprite)) #f n))
            (set! laser-x -1)
            (set! laser-y -1)))))))

;; ----------------------------------------------------

(define (game-loop)
  (cls)

  ; game world
  (draw-score)
  (draw-player)
  (draw-mushrooms)
  (draw-centipedes)
  (draw-spider 10 10)

  (move-centipedes)
  
  (when (laser?)
    (draw-laser)
    (move-laser)
    (shoot-mushroom))

  ; player movement
  (let ([dx 150]
        [dy 100]
        [max-x (- (width) 5)]
        [max-y (- (height) 8)]
        [min-y (- (height) player-area)])
    (when (btn-left #t)
      (set! player-x (max (- player-x (* dx (frametime))) 0)))
    (when (btn-right #t)
      (set! player-x (min (+ player-x (* dx (frametime))) max-x)))
    (when (btn-up #t)
      (set! player-y (max (- player-y (* dy (frametime))) min-y)))
    (when (btn-down #t)
      (set! player-y (min (+ player-y (* dy (frametime))) max-y))))

  ; shooting
  (unless (laser?)
    (when (or (btn-z #t) (btn-x #t))
      (set! laser-x (+ (exact-round player-x) 2))
      (set! laser-y (- (exact-round player-y) 2))))

  ; handle quit
  (when (btn-quit)
    (quit)))

;; ----------------------------------------------------

(define (random-mushrooms)
  (for ([i (range (exact-floor (* 0.1 cells-wide cells-high)))])
    (let ([x (random cells-wide)]
          [y (random cells-high)])
      (vector-set! mushrooms (+ (* y cells-wide) x) 0))))

;; ----------------------------------------------------

(define (new-game)
  (set! level 1)
  (set! score 0)
  (set! player-x (- (/ (width) 2) 3))
  (set! player-y (- (height) 8))
  (set! mushrooms (make-vector (* cells-wide cells-high) #f))

  ; initial centipede
  (spawn-centipede 8)

  ; setup random initial board
  (random-mushrooms))

;; ----------------------------------------------------

(define (play)
  (let ([w (* cells-wide 6)]
        [h (+ (* (+ cells-high 1) 6) y-offset 8)])
    (run game-loop w h #:init new-game)))

;; ----------------------------------------------------

(play)
