#lang racket

(require r-cade)

;; ----------------------------------------------------

(define gem-sprites
  #((#x3c #x7e #xff #xff #xff #xff #x7e #x3c)
    (#x7e #xff #xff #xff #xff #xff #xff #x7e)
    (#x18 #x3c #x7e #xff #xff #x7e #x3c #x18)
    (#xff #xff #xff #xff #xff #xff #xff #xff)))

;; ----------------------------------------------------

(define gem-hilites
  #((#x00 #x18 #x20 #x40 #x40 #x00)
    (#x00 #x3c #x40 #x40 #x40 #x40)
    (#x00 #x18 #x20 #x40 #x40 #x00)
    (#x00 #x00 #x00 #x00 #x00 #x00)))

;; ----------------------------------------------------

(define ribbon-sprite
  '(#xffff #x8094 #xb6b6 #x88c1 #xb6b6 #x8094 #xffff))

;; ----------------------------------------------------

(define gem-colors #(8 12 10 11 9 14))

;; ----------------------------------------------------

(define theme-voice (voice (synth (sin 0.5)
                                  (sin 0.2)
                                  (triangle-wave -0.2))
                           (envelope 1 0.5 0.25 0.125 0.2 0.1 0.2 0.1)))

;; ----------------------------------------------------

(define thud-sound (tone 50 0.35 (voice (synth (square-wave 0.6) (sawtooth-wave 0.3)) fade-out-envelope)))
(define clear-sound (tone 600 0.3 theme-voice))
(define dead-sound (tone 100 1 (voice (synth (noise-wave 0.7) (sawtooth-wave 0.5)) fade-out-envelope)))

;; ----------------------------------------------------

;; see: https://www.youtube.com/watch?v=Nsf9XTbpoOI

(define theme
  (music (string-append
          "A3EC4E3BEAE"
          "G#EBEAEG#E"
          "GDBDADGD"
          "F#DGDADGD"
          "AEC4E3BEAE"
          "D4E3BEG#EBE"
          "GDBDADC4D3"
          "BDGDF#DGD"
          "AECEAEAE"
          "G#3EB2E3G#C4B3"
          "GF#GABAGA"
          "F#GAF#DEF#D"
          "E4-CDE-D-" ; bar 13
          "E-D-B3BD4B3"
          "BC4DB3C4DEC"
          "A3BC4A3D4CB3C4"
          "D4-------"
          "B3C4DG#3ABC4F#3"
          "BGEAG-F#-"
          "E3AF#DEAF#D"
          "D4B3C4B3A---"
          "B3C4DG#3ABC4F#3"  ; bar 22
          "GBAGF#AGF#"
          "EAF#DEAF#D"
          "EAF#DE---"
          "ABC4F3--------"
          "G-F-E-------"
          "ABC4F3----"
          "D4-C-B3-----------"
          "E4CDB3C4A3G#A" ; bar 33
          "B-------"
          "G#ABG#ABG#A"
          "ABC4A3BC4A3B"
          "BC4DB3C4DB3C4"
          "DECDECDE"
          "G#3ABG#ABG#A"
          "ABC4A3BC4A3B"
          "E4CDB3G#--A"
          "A-------")
         #:tempo 240
         #:voice theme-voice))

;; ----------------------------------------------------

(define warning-theme
  (music (string-append
          "E4CA3E4CA3E4CA3E4CA3"
          "D4B3GD4B3GD4B3GD4B3G"
          "C4A3FC4A3FD4B3GEGB"
          "D3FAC4A3C4E3AC4ECA3"
          "E4CA3E4CA3E4CA3E4CA3"
          "D4B3GD4B3GD4B3GD4B3G"
          "F3AC4EDCB3C4B3ABC4"
          "B3C4DEDCB3C4B3AGF"
          "E3GBEGBEGBEGB"
          "DF#ADF#ADF#ADF#A"
          "F#3AC4F#3AC4F#3AC4F#3AC4"
          "E3GBE3GBE3GBC4B3A"
          "E4CA3E4CA3E4CA3E4CA3"
          "D4B3GD4B3GD4B3GD4B3G"
          "A3FAC4A3C4DCB3ABA"
          "G#G#G#AA-------"
          )
         #:tempo 360
         #:voice theme-voice))

;; ----------------------------------------------------

(struct column [gems (y-shift #:mutable)])
(struct gem [color sprite])

;; ----------------------------------------------------

(define end-game-gem (gem 5 3))

;; ----------------------------------------------------

(define board-x 4)
(define board-y 126)

;; ----------------------------------------------------

(define columns #f)

;; ----------------------------------------------------

(define triplet #f)
(define triplet-x #f)
(define triplet-y #f)
(define triplet-shift #f)
(define triplet-next #f)

;; ----------------------------------------------------

(define drop-frame 0)
(define multiplier 1)
(define difficulty 1)
(define score 0)
(define jewels 0)

;; ----------------------------------------------------

(define danger-zone #f)

;; ----------------------------------------------------

(define rotate-action (action btn-z #t 2))
(define drop-action (action btn-down #t 15))
(define left-action (action btn-left #t 5))
(define right-action (action btn-right #t 5))

;; ----------------------------------------------------

(define match-sets
  (append

   ; all horizontals
   (for*/list ([x (range 4)]
               [y (range 16)])
     (list (list x y) (list (+ x 1) y) (list (+ x 2) y)))

   ; all verticals
   (for*/list ([x (range 6)]
               [y (range 14)])
     (list (list x y) (list x (+ y 1)) (list x (+ y 2))))

   ; all top-left -> bottom-right diagonals
   (for*/list ([x (range 4)]
               [y (range 2 16)])
     (list (list x y) (list (+ x 1) (- y 1)) (list (+ x 2) (- y 2))))

   ; all bottom-left -> top-right diagonals
   (for*/list ([x (range 4)]
               [y (range 14)])
     (list (list x y) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 2))))))

;; ----------------------------------------------------

(define (setup-board)
   (set! columns (for/vector ([x (range 6)])
                   (column (make-vector 15 #f) #f))))

;; ----------------------------------------------------

(define (drop-gems col)
  (let ([gems (column-gems col)]
        [y (column-y-shift col)])
    (vector-copy! gems y gems (+ y 1))

    ; clear the top gem from the column
    (vector-set! gems (- (vector-length gems) 1) #f)

    ; clear the y offset
    (set-column-y-shift! col #f)))

;; ----------------------------------------------------

(define (drop-column col)
  (if (column-y-shift col)
      (drop-gems col)

      ; find the next hole in the column
      (let* ([gems (column-gems col)]
             [y (vector-member #f gems)])

        ; set the y-offset to the hole when gems are above it
        (when (and y (for/or ([i (range y (vector-length gems))])
                       (vector-ref gems i)))
          (set-column-y-shift! col y)))))

;; ----------------------------------------------------

(define (board-pixel x y #:shift [shift #f])
  (let ([offset (if shift 4 8)])
    (values (+ board-x (* x 9) 1)
            (- board-y (* y 9) offset))))

;; ----------------------------------------------------

(define (draw-floating-gem x y gem #:color [c (gem-color gem)])
  (color c)
  (draw x y (vector-ref gem-sprites (gem-sprite gem)))
  (color 7)
  (draw x y (vector-ref gem-hilites (gem-sprite gem))))

;; ----------------------------------------------------

(define (draw-gem x y gem #:shift [shift #f] #:color [c (gem-color gem)])
  (let-values ([(gx gy) (board-pixel x y #:shift shift)])
    (draw-floating-gem gx (+ gy 1) gem #:color c)))

;; ----------------------------------------------------

(define (draw-board)
  (color 0)
  (rect board-x (- board-y (* 13 9) -1) (* 6 9) (* 13 9) #:fill #t)
  
  ; draw the grid
  (color 1)
  (for* ([x (range 6)]
         [y (range 13)])
    (let-values ([(x1 y1) (board-pixel x y)])
      (let ([x2 (+ x1 7)]
            [y2 (+ y1 8)])
        (line x1 y1 x2 y1)
        (line x1 y1 x1 y2))))

  ; draw the columns
  (for ([col columns] [x (in-naturals)])
    (for ([gem (column-gems col)] [y (in-naturals)] #:when gem)
      (let ([shift (column-y-shift col)])
        (draw-gem x y gem #:shift (and shift (< shift y)))))))

;; ----------------------------------------------------

(define (draw-triplet gems x y #:shift [shift #f])
  (let-values ([(rx ry) (board-pixel x y #:shift shift)])
    (color 0)
    (rect (- rx 1) ry 10 28 #:fill #t)

    ; the gems
    (for ([gem gems] [i (in-naturals)])
      (draw-gem x (- y i) gem #:shift shift))

    ; outline the triplet
    (color 7)
    (rect (- rx 1) ry 10 28)))

;; ----------------------------------------------------

(define (gem-at x y)
  (and (<= 0 x 5)
       (<= 0 y 12)
       (vector-ref (column-gems (vector-ref columns x)) y)))

;; ----------------------------------------------------

(define (set-gem-at! x y gem)
  (let ([gems (column-gems (vector-ref columns x))])
    (vector-set! gems y gem)))

;; ----------------------------------------------------

(define (flash-matches matches)
  (for ([c (range 8)])
    (for ([m matches])
      (let* ([x (first m)]
             [y (second m)]
             [g (gem-at x y)])
        (draw-gem x y g #:color (+ 8 c))))
    (sync)))

;; ----------------------------------------------------

(define (clear-matches)
  (let ([matches null])

    ; find all cells that should be cleared
    (for ([m match-sets])
      (let ([gem (apply gem-at (first m))])
        (when gem
          (let ([c (gem-color gem)])
            (when (and (not (= c (gem-color end-game-gem)))
                       (andmap (λ (pos)
                                 (let ([g (apply gem-at pos)])
                                   (and g (= (gem-color g) c))))
                               (rest m)))
              (set! matches (append m matches)))))))

    ; return the matches
    (remove-duplicates matches)))

;; ----------------------------------------------------

(define (spawn-triplet)
  (set! triplet triplet-next)
  (set! triplet-next (random-triplet))

  ; the initial y-offset of the triplet and shift
  (set! triplet-x 3)
  (set! triplet-y 15)
  (set! triplet-shift #t))

;; ----------------------------------------------------

(define (play-drop-anim)
  (for ([col columns]) (drop-column col))
  (draw-game #f)
  (sync)

  ; are any of the columns falling?
  (when (for/or ([col columns])
          (column-y-shift col))
    (for ([col columns]) (drop-column col))
    (draw-game #f)
    (sync)
    (play-drop-anim)))

;; ----------------------------------------------------

(define (lock-triplet)
  (play-sound thud-sound)

  ; stick the gems to the columns
  (for ([gem triplet] [y (in-naturals)])
    (set-gem-at! triplet-x (- triplet-y y) gem))

  ; try to clear all the matches
  (do [(matches (clear-matches)
                (clear-matches))]
    [(empty? matches) (void)]

    ; play chime sound
    (play-sound clear-sound)

    ; flash and clear
    (flash-matches matches)
    (for ([pos matches])
      (set-gem-at! (first pos) (second pos) #f))

    ; score the matches and add score multiplier
    (let ([n (length matches)])
      (set! score (+ score (* n 50 multiplier)))
      (set! jewels (+ jewels n))
      (set! multiplier (+ multiplier 1)))

    ; drop the columns
    (play-drop-anim))

  ; check for game over
  (if (for/or ([col columns])
        (vector-ref (column-gems col) 13))
      (game-over)
      (begin
        (spawn-triplet)

        ; reset the score multiplier
        (set! multiplier 1))))

;; ----------------------------------------------------

(define (can-drop-triplet)
  (or triplet-shift
      (and (> triplet-y 2)
           (not (gem-at triplet-x (- triplet-y 3))))))

;; ----------------------------------------------------

(define (drop-triplet)
  (if (can-drop-triplet)
      (if triplet-shift
          (begin
            (set! triplet-y (- triplet-y 1))
            (set! triplet-shift #f))
          (set! triplet-shift #t))
      (lock-triplet)))

;; ----------------------------------------------------

(define (random-gem)
  (let ([i (random (+ (vector-length gem-colors) -3 difficulty))])
    (gem (vector-ref gem-colors i) (remainder i 3))))
  
;; ----------------------------------------------------

(define (random-triplet)
  (list (random-gem)
        (random-gem)
        (random-gem)))

;; ----------------------------------------------------

(define (rotate-triplet [inverse #f])
  (set! triplet (if inverse
                    (list (second triplet)
                          (third triplet)
                          (first triplet))
                    (list (third triplet)
                          (first triplet)
                          (second triplet)))))

;; ----------------------------------------------------

(define (move-triplet dx)
  (when (and (<= 0 (+ triplet-x dx) 5)
             (let ([y-range (range (- triplet-y (if triplet-shift 3 2)) (+ triplet-y 1))])
               (for/and ([y y-range])
                 (not (gem-at (+ triplet-x dx) y)))))
    (set! triplet-x (+ triplet-x dx))))

;; ----------------------------------------------------

(define (level)
  (+ (quotient jewels 20)
     (case difficulty
       [(1) 0]
       [(2) 5]
       [(3) 10])))

;; ----------------------------------------------------

(define (frame-to-drop)
  (max 2 (- 20 (level))))

;; ----------------------------------------------------

(define (max-height)
  (apply max (for/list ([col columns])
               (for/fold ([n 0])
                         ([g (column-gems col)]
                          [i (in-naturals)])
                 (if g i n)))))

;; ----------------------------------------------------

(define (game-over)
  (stop-music)
  (play-sound dead-sound)

  ; fill the board
  (for ([y (range 14 -1 -1)])
    (for ([col columns])
      (vector-set! (column-gems col) y (if (> y 12) #f end-game-gem)))
    (when (< y 13)
      (draw-game #f)
      (sync)))

  ; show end of game
  (let-values ([(x y) (board-pixel 1 7)])
    (color 0)
    (text x (+ y 3) "GAME OVER")
    (color 7)
    (text x (+ y 2) "GAME OVER"))

  ; done
  (wait btn-start)
  (if (btn-start)
      (goto start-screen)
      (quit)))

;; ----------------------------------------------------

(define (clear-columns)
  (for ([y (range 12 -1 -1)])
    (for ([col columns])
      (vector-set! (column-gems col) y #f))
    (draw-game #f)
    (sync)))

;; ----------------------------------------------------

(define (new-game)
  (setup-board)

  ; fill lines with random gems
  (for* ([y (range (case difficulty
                    [(1) 0]
                    [(2) 2]
                    [(3) 4]))]
         [col columns])
    (vector-set! (column-gems col) y (random-gem)))

  ; the initial and next gem triplets
  (set! triplet-next (random-triplet))
  (spawn-triplet)

  ; no lock delay and zero score
  (set! score (case difficulty
                [(1) 0]
                [(2) 5000]
                [(3) 10000]))
  (set! drop-frame 0)
  (set! multiplier 1)
  (set! jewels 0)
  (set! danger-zone #f))

;; ----------------------------------------------------

(define (draw-ribbon)
  (for ([x (range (- (bitwise-and (frame) #xf)) (+ (width) 16) 16)])
    (color 0)
    (draw-ex x 1 ribbon-sprite)
    (draw-ex x (- (height) 6) ribbon-sprite)
    (color 10)
    (draw-ex x 0 ribbon-sprite)
    (draw-ex x (- (height) 7) ribbon-sprite)))

;; ----------------------------------------------------

(define (draw-game [show-triplet #t])
  (cls 4)
  (draw-board)

  ; optionally show next
  (when triplet-next
    (draw-triplet triplet-next 7 11))

  ; render the scrolling ribbon
  (draw-ribbon)

  ; current piece
  (when show-triplet
    (draw-triplet triplet triplet-x triplet-y #:shift triplet-shift))

  ; UI
  (let-values ([(x y) (board-pixel 7 6)])
    (color 0)
    (font basic-font)
    (text x (+ y 1) "SCORE")
    (text x (+ y 21) "LEVEL")
    (text x (+ y 41) "JEWELS")
    (text (+ x 4) (+ y 8) score)
    (text (+ x 4) (+ y 28) (level))
    (text (+ x 4) (+ y 48) jewels)
    (color 9)
    (text x y "SCORE")
    (text x (+ y 20) "LEVEL")
    (text x (+ y 40) "JEWELS")
    (color 7)
    (text (+ x 4) (+ y 7) score)
    (text (+ x 4) (+ y 27) (level))
    (text (+ x 4) (+ y 47) jewels)))

;; ----------------------------------------------------

(define (start-screen)
  (let ([gems (for*/list ([x (range 2 (- (width) 8) 8)])
                (list x (- -24 (random (* (height) 2))) 0 (random-triplet)))])
    (goto (λ ()
            (cls)

            ; draw falling gems
            (set! gems (for/list ([g gems])
                         (match g
                           [(list x y dy triplet)
                            (draw-floating-gem x y (first triplet))
                            (draw-floating-gem x (- y 8) (second triplet))
                            (draw-floating-gem x (- y 16) (third triplet))

                            ; drop the gems
                            (let ([ny (+ y (* dy (frametime)))]
                                  [nd (+ dy (* 60 (frametime)))])
                              (if (> ny (+ (height) 16))
                                  (list x (- -24 (random (* (height) 2))) 0 (random-triplet))
                                  (list x ny nd triplet)))])))

            ; ribbon
            (draw-ribbon)

            ; show the game title
            (let ([x (remainder (frame) (+ (width) 56))])
              (color (+ 8 (remainder (frame) 8)))
              (font wide-font)
              (text (- x 56) 40 "COLUMNS")
              (text (- (width) x) 40 "COLUMNS"))

            ; show the difficulties
            (color 7)
            (font basic-font)
            (text 34 62 (format "~a Easy" (if (= difficulty 1) "*" " ")))
            (text 34 70 (format "~a Medium" (if (= difficulty 2) "*" " ")))
            (text 34 78 (format "~a Hard" (if (= difficulty 3) "*" " ")))

            ; show the PRESS START command
            (when (< (remainder (frame) 10) 5)
              (color 7)
              (font tall-font)
              (text 18 96 "Press START"))

            ; select difficulty
            (when (or (btn-down) (btn-select))
              (set! difficulty (+ (remainder difficulty 3) 1)))
            (when (btn-up)
              (set! difficulty (+ (remainder (+ difficulty 1) 3) 1)))

            ; start game
            (when (btn-start)
              (play-music theme)
              (new-game)
              (goto game-loop))))))

;; ----------------------------------------------------

(define (game-loop)
  (draw-game)

  ; drop the triplet
  (set! drop-frame (+ drop-frame 1))
  (when (or (drop-action) (> drop-frame (frame-to-drop)))
    (set! drop-frame 0)
    (drop-triplet))

  ; swap music theme
  (set! danger-zone (cond
                      [(and danger-zone (< (max-height) 7))
                       (begin0 #f (play-music theme))]
                      [(and (not danger-zone) (> (max-height) 7))
                       (begin0 #t (play-music warning-theme))]
                      [else danger-zone]))

  ; player controls
  (when (rotate-action)
    (rotate-triplet))
  (when (left-action)
    (move-triplet -1))
  (when (right-action)
    (move-triplet +1)))

;; ----------------------------------------------------

(define (play)
  (run start-screen 100 137 #:init new-game #:fps 30 #:title "R-cade: Columns"))

;; ----------------------------------------------------

(module+ main
  (play))
