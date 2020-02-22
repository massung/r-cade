#lang racket

(require r-cade)

;; ----------------------------------------------------

(struct game-obj [(x #:mutable) (y #:mutable) sprite])
(struct ship game-obj [color])
(struct bullet game-obj [dx dy])
(struct star game-obj [dx])
(struct boom game-obj [anim])

;; ----------------------------------------------------

(define player (ship 0 0 '(#x60 #x76 #xfc #x7c #xfc #x76 #x60) 2))

;; ----------------------------------------------------

(define player-speed 1)

;; ----------------------------------------------------

(define shoot-sound (sweep 800 600 0.15 (voice square-wave fade-out-envelope)))
(define boom-sound (tone 50 2.0 (voice noise-wave fade-out-envelope)))

;; ----------------------------------------------------

(define ships null)
(define bullets null)
(define stars null)
(define booms null)

;; ----------------------------------------------------

(define-action move-up btn-up)
(define-action move-down btn-down)
(define-action move-right btn-right)
(define-action move-left btn-left)

;; ----------------------------------------------------

(define-action fire btn-z 6)

;; ----------------------------------------------------

(define (spawn-bullet b)
  (set! bullets (cons b bullets)))

;; ----------------------------------------------------

(define (spawn-stars [n 30])
  (set! stars (for/list ([i (range n)])
                (star (random 256) (random 128) '(#x80) (random)))))

;; ----------------------------------------------------

(define (draw-game-obj obj)
  (draw (game-obj-x obj) (game-obj-y obj) (game-obj-sprite obj)))

;; ----------------------------------------------------

(define (draw-ship ship)
  (color (ship-color ship))
  (draw-game-obj ship))

;; ----------------------------------------------------

(define (draw-player)
  (draw-ship player))

;; ----------------------------------------------------

(define (draw-bullets)
  (color 10)
  (for ([b bullets])
    (draw-game-obj b)))

;; ----------------------------------------------------

(define (draw-stars)
  (color 6)
  (for ([star stars])
    (draw-game-obj star)))

;; ----------------------------------------------------

(define (advance-bullets)
  (set! bullets (for/list ([b bullets] #:when (and (< -8 (game-obj-x b) (width))
                                                   (< -1 (game-obj-y b) (height))))
                  (set-game-obj-x! b (+ (game-obj-x b) (bullet-dx b)))
                  (set-game-obj-y! b (+ (game-obj-y b) (bullet-dy b)))

                  ; keep bullets on screen
                  b)))

;; ----------------------------------------------------

(define (scroll-stars)
  (for ([star stars])
    (let ([nx (- (game-obj-x star) (star-dx star))])
      (when (< nx -1)
        (set! nx (+ (width) (random (width))))
        (set-game-obj-y! star (random (height))))
      (set-game-obj-x! star nx))))

;; ----------------------------------------------------

(define (shoot-bullet from-ship dx dy)
  (let ([x (game-obj-x from-ship)]
        [y (game-obj-y from-ship)])
    (spawn-bullet (if (> dx 0)  ; player bullets move to the right
                      (bullet (+ x 5) (+ y 1) '(#xe0 #x00 #x00 #x00 #xe0) dx dy)
                      (bullet (- x 1) (+ y 3) '(#xf0) dx dy)))))

;; ----------------------------------------------------

(define (setup)
  (spawn-stars)

  ; clear object lists
  (set! bullets null)
  (set! booms null)

  ; reset the player
  (set-game-obj-x! player 16)
  (set-game-obj-y! player 62))

;; ----------------------------------------------------

(define (new-game)
  (setup)
  
  ; main game loop
  (λ ()
    (cls)

    ; move bullets, collide with ships
    (advance-bullets)
    (scroll-stars)

    ; draw the player, enemies, and all bullets
    (draw-stars)
    (draw-player)
    (draw-bullets)

    ; move the player around
    (when (move-up)
      (set-game-obj-y! player (- (game-obj-y player) player-speed)))
    (when (move-down)
      (set-game-obj-y! player (+ (game-obj-y player) player-speed)))
    (when (move-right)
      (set-game-obj-x! player (+ (game-obj-x player) player-speed)))
    (when (move-left)
      (set-game-obj-x! player (- (game-obj-x player) player-speed)))

    ; clamp player motion
    (set-game-obj-x! player (max (game-obj-x player) 0))

    ; let the player shoot
    (when (fire)
      (play-sound shoot-sound)
      (shoot-bullet player 3 0))

    ; check for game quit
    (when (btn-quit)
      (quit))))

;; ----------------------------------------------------

(define (play)
  (run (new-game) 256 128 #:title "Shooter"))
