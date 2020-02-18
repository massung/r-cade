#lang racket

(require r-cade)

;; ----------------------------------------------------

(define colors #(8 11 9 14 13 12 10))

;; ----------------------------------------------------

(define tetrinome-bits
  #(#((#b1100 #b0110 #b0000)        ; Z
      (#b0010 #b0110 #b0100)
      (#b0000 #b1100 #b0110)
      (#b0100 #b1100 #b1000))
    #((#b0110 #b1100 #b0000)        ; S
      (#b0100 #b0110 #b0010)
      (#b0000 #b0110 #b1100)
      (#b1000 #b1100 #b0100))
    #((#b0000 #b1110 #b1000)        ; L
      (#b1100 #b0100 #b0100)
      (#b0010 #b1110 #b0000)
      (#b0100 #b0100 #b0110))
    #((#b0100 #b1110 #b0000)        ; T
      (#b0100 #b0110 #b0100)
      (#b0000 #b1110 #b0100)
      (#b0100 #b1100 #b0100))
    #((#b1000 #b1110 #b0000)        ; J
      (#b0110 #b0100 #b0100)
      (#b0000 #b1110 #b0010)
      (#b0100 #b0100 #b1100))
    #((#b0000 #b1111 #b0000 #b0000) ; I
      (#b0010 #b0010 #b0010 #b0010)
      (#b0000 #b0000 #b1111 #b0000)
      (#b0100 #b0100 #b0100 #b0100))
    #((#b0000 #b0110 #b0110 #b0000) ; O
      (#b0000 #b0110 #b0110 #b0000)
      (#b0000 #b0110 #b0110 #b0000)
      (#b0000 #b0110 #b0110 #b0000))))

;; ----------------------------------------------------

(define tetrinomes
  (for/vector ([shapes tetrinome-bits])
    (for/vector ([shape shapes])
      (flatten (for/list ([bits shape])
                 (let ([b (bitwise-ior
                           (if (bitwise-bit-set? bits 3) #xc0 0)
                           (if (bitwise-bit-set? bits 2) #x30 0)
                           (if (bitwise-bit-set? bits 1) #x0c 0)
                           (if (bitwise-bit-set? bits 0) #x03 0))])
                   (list b b)))))))

;; ----------------------------------------------------

(define (tetrinome shape [angle 0])
  (vector-ref (vector-ref tetrinomes shape) angle))

;; ----------------------------------------------------

(define piece #f)
(define piece-x 4)
(define piece-y 0)
(define angle 0)
(define score 0)
(define lines 0)

;; ----------------------------------------------------

(define chime (sound (tone 2000) 0.2 (voice triangle-wave peak-envelope)))

;; ----------------------------------------------------

(define (level)
  (quotient lines 10))

(define (drop-rate)
  (max (- 1.0 (* (level) 0.08)) 0.1))

;; ----------------------------------------------------

(define drop-timer 0.0)

;; ----------------------------------------------------

(define (shuffle-pieces)
  (shuffle (flatten (for/list [(i (range 7))] (list i i i)))))

;; ----------------------------------------------------

(define piece-order (shuffle-pieces))

;; ----------------------------------------------------

(define board (make-vector (* 10 20) #f))

;; ----------------------------------------------------

(define (board-ref x y)
  (cond
    [(or (< x 0) (>= x 10) (>= y 20)) #t]
    [(< y 0) #f]
    [else (vector-ref board (+ (* y 10) x))]))

;; ----------------------------------------------------

(define (board-ref! x y n)
  (unless (< y 0)
    (vector-set! board (+ (* y 10) x) n)))

;; ----------------------------------------------------

(define (line-clear? y)
  (for/and ([x (range 10)])
    (board-ref x y)))

;; ----------------------------------------------------

(define (draw-board x y)
  (color 6)
  (rect x (+ y 1) 20 39)

  ; draw the pieces on the board
  (color 6)
  (for ([bx (range 10)])
    (for ([by (range 20)])
      (let ([n (board-ref bx by)])
        (color (if n (vector-ref colors n) 1))
        (draw (+ x (* bx 2)) (+ y (* by 2)) '(#xc0 #xc0))))))

;; ----------------------------------------------------

(define (test-shape x y n r)
  (let ([shape (vector-ref (vector-ref tetrinome-bits n) r)])
    (for/or ([dy (range 4)] [bits shape])
      (for/or ([dx (range 4)])
        (let ([b (bitwise-bit-set? bits (- 3 dx))])
          (and b (board-ref (+ x dx) (+ y dy))))))))

;; ----------------------------------------------------

(define (spawn-tetrinome)
  (let* ([n (first piece-order)]
         [t (tetrinome n)])
    (set! piece-order (rest piece-order))
    (when (empty? piece-order)
      (set! piece-order (shuffle-pieces)))
    (set! piece-x 3)
    (set! piece-y -2)
    (set! piece n)
    (set! angle 0)))

;; ----------------------------------------------------

(define (place-tetrinome)
  (let ([shape (vector-ref (vector-ref tetrinome-bits piece) angle)])
    (for ([bits shape] [y (range 4)])
      (when (bitwise-bit-set? bits 3)
        (board-ref! (+ piece-x 0) (+ piece-y y) piece))
      (when (bitwise-bit-set? bits 2)
        (board-ref! (+ piece-x 1) (+ piece-y y) piece))
      (when (bitwise-bit-set? bits 1)
        (board-ref! (+ piece-x 2) (+ piece-y y) piece))
      (when (bitwise-bit-set? bits 0)
        (board-ref! (+ piece-x 3) (+ piece-y y) piece)))))

;; ----------------------------------------------------

(define (clear-lines)
  (let [(n (for/sum ([y (range 20)] #:when (line-clear? y))
             (vector-copy! board 10 board 0 (* y 10))
             1))]
    (when (> n 0)
      (play-sound chime))
    (set! lines (+ lines n))
    (set! score (+ score (* 20 n n)))))

;; ----------------------------------------------------

(define (draw-tetrinome x y n r)
  (color (vector-ref colors n))
  (draw x y (vector-ref (vector-ref tetrinomes n) r)))

;; ----------------------------------------------------

(define (game-over?)
  (for/or ([x 10]) (board-ref x 0)))

;; ----------------------------------------------------

(define (draw-game-over)
  (let* ([s (~a score)]
         [n (string-length s)]
         [x (/ (- (width) (* n 4)) 2)])
    (color 7)
    (text 7 16 "GAME OVER")
    (text (+ x 1) 24 score)))

;; ----------------------------------------------------

(define (draw-game)
  (let ([board-x 2]
        [board-y 10])
    (draw-board board-x board-y)

    ; draw the current piece on the board
    (let ([x (+ board-x (* piece-x 2))]
          [y (+ board-y (* piece-y 2))])
      (draw-tetrinome x y piece angle)))

  ; status
  (color 7)
  (text 27 1 "Score")
  (text 27 19 "Lines")
  (text 27 37 "Level")

  ; stats
  (color 10)
  (text 27 8 score)
  (text 27 26 lines)
  (text 27 45 (+ (level) 1))
  
  ; draw the next piece
  (draw-tetrinome 1 1 (first piece-order) 0))

;; ----------------------------------------------------

(define-action spin-block btn-up 5)
(define-action move-left btn-left 7)
(define-action move-right btn-right 7)
(define-action move-down btn-down)

;; ----------------------------------------------------

(define (tetris)
  (cls)

  ; test for game over
  (if (game-over?)
      (draw-game-over)
      (begin
        (draw-game)
  
        ; descend?
        (set! drop-timer (+ drop-timer (frametime)))
        (when (or (move-down) (> drop-timer (drop-rate)))
          (if (test-shape piece-x (+ piece-y 1) piece angle)
              (begin
                (place-tetrinome)
                (clear-lines)
                (spawn-tetrinome))
              (set! piece-y (+ piece-y 1)))

          ; reset the drop timer
          (set! drop-timer 0.0))

        ; rotate the piece
        (when (spin-block)
          (let ([new-angle (bitwise-and (add1 angle) 3)])
            (unless (test-shape piece-x piece-y piece new-angle)
              (set! angle new-angle))))

        ; move the piece side-to-side
        (when (move-left)
          (unless (test-shape (- piece-x 1) piece-y piece angle)
            (set! piece-x (- piece-x 1))))
        (when (move-right)
          (unless (test-shape (+ piece-x 1) piece-y piece angle)
            (set! piece-x (+ piece-x 1))))))

  ; quit the game?
  (when (btn-quit)
    (quit)))

;; ----------------------------------------------------

(define theme (music ".--E4-B3C4D-CB3A-AC4E-DCB3-C4D-E-C-A3-A-.D4-FA-GFE-CE-DCB3-BC4D-E-C-A3-A-.E4---C---D---B3---C4---A3---Ab---B-E4---C---D---B3--C4--E-A4--Ab---.E4-B3C4D-CB3A-AC4E-DCB3-C4D-E-C-A3-A-.D4-FA-GFE-CE-DCB3-BC4D-E-C-A3-A-"
                     #:tempo 280))

;; ----------------------------------------------------

(define (new-game)
  (spawn-tetrinome)
  (play-music theme))
  
;; ----------------------------------------------------

(define (play)
  (run tetris 48 52 #:init new-game #:fps 30 #:title "R-cade: Tetris"))
