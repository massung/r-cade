#lang racket

(require r-cade)
(require racket/random)

;; ----------------------------------------------------

(define city-sprite '(#x20 #x68 #xed #xff #xff))
(define destroyed-sprite '(#x00 #x00 #x00 #x91 #xd3))
(define silo-sprite '(#x40 #x40 #x40 #xe0 #xa0))
(define redicule-sprite '(#x20 #x20 #xd8 #x20 #x20))
(define target-sprite '(#x88 #x50 #x20 #x50 #x88))

;; ----------------------------------------------------

(define boom-sound (tone 50 1.25 (voice noise-wave fade-out-envelope)))

;; ----------------------------------------------------

(define launch-sound (sweep 600 400 0.75 (voice sawtooth-wave fade-out-envelope)))

;; ----------------------------------------------------

(struct building [pos (alive #:mutable)])
(struct silo building [(missile #:mutable)])

;; ----------------------------------------------------

(struct missile [x1 y1 x2 y2 (u #:mutable) du (live #:mutable) target (mirv #:mutable)])
(struct explosion [x y (r #:mutable) (dr #:mutable)])

;; ----------------------------------------------------

(define cities null)
(define silos null)
(define enemy-missiles null)
(define player-missiles null)
(define enemy-explosions null)
(define player-explosions null)

;; ----------------------------------------------------

(define wave 0)
(define score 0)
(define multiplier 0)
(define missile-count 0)

;; ----------------------------------------------------

(define launch-btn (action btn-mouse))

;; ----------------------------------------------------

(define (dist-sq building x y)
  (let ([dx (- (building-pos building) x)]
        [dy (- 120 y)])
    (+ (* dx dx) (* dy dy))))

;; ----------------------------------------------------

(define (color-frame)
  (color (+ 8 (bitwise-and (frame) #x7))))

;; ----------------------------------------------------

(define (score-pts n)
  (set! score (+ score (* n multiplier))))

;; ----------------------------------------------------

(define (draw-ground)
  (color 2)
  (rect 0 124 (width) 4 #:fill #t))

;; ----------------------------------------------------

(define (draw-cities)
  (for ([city cities])
    (let ([x (- (building-pos city) 4)])
      (if (building-alive city)
          (begin
            (color 12)
            (draw x 120 city-sprite))
          (begin
            (color 4)
            (draw x 120 destroyed-sprite))))))

;; ----------------------------------------------------
(define (draw-silos)
  (color 9)
  (for ([silo silos] #:when (building-alive silo))
    (let ([x (building-pos silo)])
      (draw (- x 1) 120 silo-sprite)
      (draw (- x 4) 122 silo-sprite)
      (draw (+ x 2) 122 silo-sprite))))

;; ----------------------------------------------------

(define (missile-pos m)
  (let ([x1 (missile-x1 m)]
        [y1 (missile-y1 m)]
        [x2 (missile-x2 m)]
        [y2 (missile-y2 m)]
        
        ; pixel distance along path (0.0 - 1.0)
        [u (missile-u m)])
    (values (+ x1 (* (- x2 x1) u))
            (+ y1 (* (- y2 y1) u)))))

;; ----------------------------------------------------

(define (draw-missile m [show-target? #f])
  (let-values ([(x y) (missile-pos m)])
    (line (missile-x1 m) (missile-y1 m) x y)

    (color 7)
    (draw (- x 1) (- y 1) '(#xc0 #xc0))

    ; show targets for player missiles
    (when show-target?
      (let ([x (missile-x2 m)]
            [y (missile-y2 m)])
        (color-frame)
        (draw (- x 2) (- y 2) target-sprite)))))

;; ----------------------------------------------------

(define (draw-missiles)
  (for ([m enemy-missiles])
    (color 8)
    (draw-missile m))
  (for ([m player-missiles])
    (color 10)
    (draw-missile m #t)))

;; ----------------------------------------------------

(define (draw-explosion e)
  (circle (explosion-x e) (explosion-y e) (explosion-r e) #:fill #t))

;; ----------------------------------------------------

(define (draw-explosions)
  (color-frame)
  (for ([e enemy-explosions])
    (draw-explosion e))
  (for ([e player-explosions])
    (draw-explosion e)))

;; ----------------------------------------------------

(define (make-missile x1 y1 x2 y2 speed [target #f] [mirv #f])
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]

         ; distance to target
         [dist (sqrt (+ (* dx dx) (* dy dy)))]

         ; normalize distance over speed to get du
         [du (/ speed dist)]

         ; color based on target
         [color (if (> y2 y1) 8 10)])
  (missile x1 y1 x2 y2 0.0 du #t target mirv)))

;; ----------------------------------------------------

(define (closest-silo x y)
  (let ([sites (filter (λ (s)
                         (and (building-alive s)
                              (let ([m (silo-missile s)])
                                (or (not m) (not (missile-live m))))))
                       silos)])
    (if (null? sites)
        #f
        (first (sort sites < #:key (λ (site) (dist-sq site x y)))))))

;; ----------------------------------------------------

(define (launch-player-missile)
  (when (> missile-count 0)
    (let* ([x (mouse-x)]
           [y (mouse-y)]
           
           ; which silo to launch from?
           [launch-site (closest-silo x y)])
      (when launch-site
        (let ([m (make-missile (building-pos launch-site) 118 x y 200)])
          (set-silo-missile! launch-site m)
          (set! player-missiles (cons m player-missiles))
          (set! missile-count (- missile-count 1)))
        (play-sound launch-sound)))))

;; ----------------------------------------------------

(define (launch-incoming-missile x y [mirv #f])
  (let ([targets (append (filter building-alive silos)
                         (filter building-alive cities))])
    (unless (null? targets)
      (let* ([target (random-ref targets)]
             [tx (building-pos target)]
             
             ; make the missile
             [m (make-missile x y tx 120 (+ 10 wave) target mirv)])
        (set! enemy-missiles (cons m enemy-missiles))))))

;; ----------------------------------------------------

(define (advance-missile m)
  (let* ([u (missile-u m)]
         [du (missile-du m)]
         
         ; new u
         [n (+ u (* du (frametime)))])
    (set-missile-u! m n)

    ; should explode (or mirv)?
    (or (and (missile-mirv m)
             (>= n 0.5))
        (>= n 1.0))))

;; ----------------------------------------------------

(define (advance-missiles)
  (for ([m player-missiles])
    (when (advance-missile m)
      (let ([e (explode-missile m)])
        (set! player-explosions (cons e player-explosions)))))

  ; enemy missiles either mirv or explode
  (for ([m enemy-missiles])
    (when (advance-missile m)

      ; it's a mirv, split off multiple warheads
      (if (missile-mirv m)
          (let-values ([(x y) (missile-pos m)])
            (for ([_ (missile-mirv m)])
              (launch-incoming-missile x y))

            ; no longer a mirv, can explode now
            (set-missile-mirv! m #f))

          ; reached target, explode!
          (let ([e (explode-missile m)])
            (set! enemy-explosions (cons e enemy-explosions))
            (set-building-alive! (missile-target m) #f)))))

  ; remove all exploded missiles
  (set! enemy-missiles (filter missile-live enemy-missiles))
  (set! player-missiles (filter missile-live player-missiles)))

;; ----------------------------------------------------

(define (explode-missile m)
  (play-sound boom-sound)
  (set-missile-live! m #f)

  ; return the created explosion so it may be added to a list
  (let-values ([(x y) (missile-pos m)])
    (explosion x y 2.0 10.0)))

;; ----------------------------------------------------

(define (advance-explosion e)
  (let ([x (explosion-x e)]
        [y (explosion-y e)]

        ; calculate new radius
        [r (+ (explosion-r e) (* (frametime) (explosion-dr e)))])
    (set-explosion-r! e r)

    ; change direction once at max radius
    (when (>= r 10.0)
      (set-explosion-dr! e -10.0))))

;; ----------------------------------------------------

(define (advance-explosions)
  (for ([e enemy-explosions])
    (advance-explosion e))

  ; player explosions can destroy enemy missiles
  (for ([e player-explosions])
    (advance-explosion e)

    ; does it collide with any enemy missiles?
    (for ([m enemy-missiles])
      (let-values ([(mx my) (missile-pos m)])
        (let* ([x (explosion-x e)]
               [y (explosion-y e)]
               [r (explosion-r e)]
               
               ; distance to missile head
               [dx (- mx x)]
               [dy (- my y)])
          (when (< (+ (* dx dx) (* dy dy)) (* r r))
            (score-pts (if (missile-mirv m) 50 25))

            ; explode the missile into the player's list
            (let ([e (explode-missile m)])
              (set! player-explosions (cons e player-explosions))))))))

  ; remove all diffused explosions
  (let ([ok (λ (e) (> (explosion-r e) 0.0))])
    (set! enemy-explosions (filter ok enemy-explosions))
    (set! player-explosions (filter ok player-explosions))))

;; ----------------------------------------------------

(define (cities-left)
  (count building-alive cities))

;; ----------------------------------------------------

(define (next-wave)
  (color 7)
  (text 60 40 (format "Ready Wave ~a" (+ wave 1)))
  (text 60 47 "Press Any Button")

  ; show bonus
  (when (> wave 0)
    (let ([missile-bonus (* missile-count 5)]
          [city-bonus (* (cities-left) 100)])
      (text 70 61 (format "x~a +~a" missile-count (* missile-bonus multiplier)))
      (text 70 68 (format "x~a +~a" (cities-left) (* city-bonus multiplier)))
      
      (color 9)
      (draw 61 61 silo-sprite)
      (draw 64 61 silo-sprite)
      (color 11)
      (draw 60 68 city-sprite)
      
      ; apply bonuses, give missiles
      (score-pts (+ missile-bonus city-bonus)))

    ; show updated score
    (draw-score))

  ; advance and give the player some missiles
  (set! wave (+ 1 wave))
  (set! missile-count (+ missile-count 10))

  ; increase score multiplier
  (when (= (bitwise-and wave 1) 1)
    (set! multiplier (+ multiplier 1)))
  
  (wait)

  ; launch incoming missiles - every nth missile is a mirv
  (for ([n (range (+ 5 wave))])
    (let ([mirv (if (zero? (remainder n 4)) (+ (random 4) 1) #f)]

          ; where is it launched from?
          [y (- (random (quotient (height) 2)))]
          [x (random (width))])
      (launch-incoming-missile x y mirv))))

;; ----------------------------------------------------

(define (draw-score)
  (color 9)
  (draw 2 9 silo-sprite)
  (color 7)
  (text 2 2 (format "Score: ~a" score))
  (text 8 9 (format "x~a" missile-count)))

;; ----------------------------------------------------

(define (game-over)
  (color 7)
  (text 40 40 (format "World lost on wave ~a" wave))
  (text 40 47 (format "Final score: ~a" score))
  (wait)
  (setup))

;; ----------------------------------------------------

(define (setup)
  (set! enemy-missiles null)
  (set! player-missiles null)
  (set! enemy-explosions null)
  (set! player-explosions null)
  
  (set! wave 0)
  (set! score 0)
  (set! multiplier 0)
  (set! missile-count 0)

  ; create silos
  (set! silos (list (silo 5 #t #f)
                    (silo 79 #t #f)
                    (silo 153 #t #f)))

  ; create cities
  (set! cities (list (building 20 #t)
                     (building 40 #t)
                     (building 60 #t)
                     (building 100 #t)
                     (building 120 #t)
                     (building 140 #t))))

;; ----------------------------------------------------

(define (game-loop)
  (cls)
  
  ; new wave?
  (when (and (null? enemy-missiles)
             (null? enemy-explosions)
             (null? player-missiles)
             (null? player-explosions))
    (if (zero? (cities-left))
        (game-over)
        (next-wave)))

  ; world
  (draw-ground)
  (draw-cities)
  (draw-silos)
  (draw-missiles)
  (draw-explosions)
  (draw-score)

  ; player
  (color 7)
  (draw (- (mouse-x) 2) (- (mouse-y) 2) redicule-sprite)
    
  ; update
  (advance-missiles)
  (advance-explosions)

  ; controls
  (when (and (launch-btn) (not (null? enemy-missiles)))
    (launch-player-missile)))

;; ----------------------------------------------------

(define (new-game)
  (hide-mouse)
  (setup))

;; ----------------------------------------------------

(define (play)
  (run game-loop 160 128 #:init new-game #:title "R-cade: Defender"))

;; ----------------------------------------------------

(module+ main
  (play))
