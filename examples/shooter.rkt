#lang racket

(require (prefix-in r: r-cade))

;; ----------------------------------------------------

(define game-obj%
  (class object%
    (super-new)

    ; initialization
    (init-field x
                y
                [pts null]
                [color 7]

                ; forward vector
                [angle 0]
                [fx 1]
                [fy 0]

                ; velocity vector
                [vx 0]
                [vy 0]

                ; angular momentum
                [w 0])

    ; other fields
    (field [age 0]
           [alive #t]

           ; radius derived from points
           [r (for/fold ([r 0])
                        ([p pts])
                (let ([dx (car p)]
                      [dy (cdr p)])
                  (max r (sqrt (+ (* dx dx) (* dy dy))))))])

    ; cached rotation matrix
    (define rx (cos (degrees->radians angle)))
    (define ry (sin (degrees->radians angle)))

    ; mark this object for destruction
    (define/public (destroy)
      (set! alive #f))

    ; rotate the object
    (define/public (turn dr)
      (set! angle (+ angle (* dr (r:frametime))))

      ; update cached matrix
      (set! rx (cos (degrees->radians angle)))
      (set! ry (sin (degrees->radians angle))))

    ; rotate and move the game object along its velocity vector
    (define/public (advance)
      (set! x (+ x (* vx (r:frametime))))
      (set! y (+ y (* vy (r:frametime))))

      ; is it offscreen and needs wrapped?
      (when (offscreen-x?)
        (set! x (- x (* (sgn vx) (+ (r:width) r r)))))
      (when (offscreen-y?)
        (set! y (- y (* (sgn vy) (+ (r:height) r r)))))

      ; apply angular momentum
      (turn (* w (r:frametime))))

    ; is the object offscreen on the x-axis?
    (define/public (offscreen-x?)
      (or (and (> vx 0) (> (- x r) (r:width)))
          (and (< vx 0) (< (+ x r) 0))))

    ; is the object offscreen on the y-axis?
    (define/public (offscreen-y?)
      (or (and (> vy 0) (> (- y r) (r:height)))
          (and (< vy 0) (< (+ y r) 0))))

    ; apply an impulse to the object in its forward vector
    (define/public (apply-impulse n)
      (let-values ([(dx dy) (rotate (* fx n) (* fy n))])
        (set! vx (+ vx dx))
        (set! vy (+ vy dy))))

    ; rotate a point into the space of this object
    (define/public (rotate px py)
      (values (- (* rx px) (* ry py))
              (+ (* ry px) (* rx py))))

    ; transform a point, return x/y multiple values
    (define/public (transform px py)
      (let-values ([(bx by) (rotate px py)])
        (values (+ x bx) (+ y by))))

    ; does this object's radius overlap another
    (define/public (overlaps? obj)
      (let ([dx (- (get-field x obj) x)]
            [dy (- (get-field y obj) y)]

            ; combine radius
            [r2 (+ (get-field r obj) r)])        
        (< (+ (* dx dx) (* dy dy)) (* r2 r2))))

    ; does any segment of this object intersect the segment passed in
    (define/public (intersects? p2.x p2.y p3.x p3.y)
      (for/fold ([p0 (first pts)] [i #f] #:result i)
                ([p1 (rest pts)])
        (values p1 (or i (let-values ([(p0.x p0.y) (transform (car p0) (cdr p0))]
                                      [(p1.x p1.y) (transform (car p1) (cdr p1))])
                           
                           ; segements p0 -> p1 and p2 -> p3
                           (let* ([s1.x (- p1.x p0.x)]
                                  [s1.y (- p1.y p0.y)]
                                  [s2.x (- p3.x p2.x)]
                                  [s2.y (- p3.y p2.y)]

                                  ; position along p0 -> p1
                                  [s (/ (- (* (- p0.y p2.y) s1.x)
                                           (* (- p0.x p2.x) s1.y))
                                        (- (* s1.x s2.y)
                                           (* s2.x s1.y)))]

                                  ; position along p2 -> p3
                                  [t (/ (- (* (- p0.y p2.y) s2.x)
                                           (* (- p0.x p2.x) s2.y))
                                        (- (* s1.x s2.y)
                                           (* s2.x s1.y)))])
                             (and (<= 0 s 1)
                                  (<= 0 t 1))))))))

    ; render the object
    (define/public (draw)
      (r:color color)

      ; polygon, transformed around origin
      (for/fold ([l (first pts)])
                ([p (rest pts)])
        (let-values ([(x1 y1) (transform (car l) (cdr l))]
                     [(x2 y2) (transform (car p) (cdr p))])
          (begin0 p (r:line x1 y1 x2 y2)))))))

;; ----------------------------------------------------

(define asteroid%
  (class game-obj%
    (super-new)
    (inherit-field x y)

    ; initialization
    (init-field [size 2])

    ; explode into smaller asteroids
    (define/public (explode)
      (when (> size 0)
        (for ([_ (range (+ 2 (random 2)))])
          (spawn-asteroid (- size 1) x y)))

      ; get rid of this asteroid
      (send this destroy))))

;; ----------------------------------------------------

(define missile%
  (class game-obj%
    (super-new [color 10])

    ; rendering fields
    (inherit-field x y color)

    ; missiles only live for a second
    (define t (r:timer 1.0))

    ; previous position used for collision detection
    (field [px x])
    (field [py y])

    ; die after time elapsed
    (define/override (advance)
      (set! px x)
      (set! py y)

      ; update position
      (super advance)

      ; destroy when timer expires
      (when (t)
        (send this destroy)))
    
    ; draw using a 1-pixel sprite
    (define/override (draw)
      (r:color color)
      (r:draw x y '(#x80)))))

;; ----------------------------------------------------

(define player-sprite '((0 . -6) (5 . 5) (0 . 1) (-5 . 5) (0 . -6)))

;; ----------------------------------------------------

(define boom-sound (r:tone 40 0.75 (r:voice r:noise-wave r:fade-out-envelope)))

;; ----------------------------------------------------

(define shoot-sound (r:sweep 600 400 0.15 (r:voice r:sawtooth-wave r:fade-out-envelope)))

;; ----------------------------------------------------

(define btn-thrust r:btn-up)
(define btn-shoot (r:action r:btn-z 6))

;; ----------------------------------------------------

(define level 0)
(define score 0)
(define hi-score 10000)
(define player #f)
(define asteroids null)
(define missiles null)
(define spawn-timer (r:timer 5.0 #:loop #t))

;; ----------------------------------------------------

(define (cross x y)
  (values y (- x)))

;; ----------------------------------------------------

(define (dot x1 y1 x2 y2)
  (+ (* x1 x2) (* y1 y2)))

;; ----------------------------------------------------

(define (random-unit-vector #:scale [s 1])
  (let ([x (random)])
    (values (* x s) (* (sqrt (- 1.0 (* x x))) s))))

;; ----------------------------------------------------

(define (random-position)
  (let ([x (random (r:width))]
        [y (random (r:height))])
    (values x y)))

;; ----------------------------------------------------

(define (random-shape n s #:variance [v 0.4])
  (let* ([random-p (λ (theta)
                     (let ([m (- (* (random) v 2) v)])
                       (cons (* (+ (* s m) s) (cos theta))
                             (* (+ (* s m) s) (sin theta)))))]

         ; initial point, loop back here
         [p (random-p 0)])

    ; rest of the polygon
    (append (cons p (for/list ([i (range 1 (- n 1))])
                      (random-p (/ (* i 2 pi) n))))
            (list p))))

;; ----------------------------------------------------

(define (spawn-asteroid [size 2] [x #f] [y #f])
  (let-values ([(fx fy) (random-unit-vector)])

    ; no position given, spawn offscreen
    (unless (and x y)
      (set! x (random (r:width)))
      (set! y (random (r:height)))

      ; move one axis offscreen
      (case (random 4)
        ((0) (set! x (- 50)))
        ((1) (set! x (+ 50 (r:width))))
        ((2) (set! y (- 50)))
        ((3) (set! y (+ 50 (r:height))))))

    ; create the object
    (let ([asteroid (new asteroid%
                         [size size]
                         [x x]
                         [y y]
                         [fx fx]
                         [fy fy]
                         [color 6]
                         [w (* 10 (- (random 360) 180))]
                         [pts (random-shape 9 (+ 6 (* size 5)))])])

      ; apply an initial force to get the asteroid moving
      (send asteroid apply-impulse (+ (random 40) 20))

      ; add it to the list
      (set! asteroids (cons asteroid asteroids)))))

;; ----------------------------------------------------

(define (launch-missile)
  (let-values ([(x y) (send player transform 0 -8)]
               [(vx vy) (send player rotate 0 -200)])
    (let ([m (new missile%
                  [x x]
                  [y y]
                  [vx vx]
                  [vy vy])])
      (r:play-sound shoot-sound)
      (set! missiles (cons m missiles)))))

;; ----------------------------------------------------

(define (test-missile-collision m a)
  (and (send a overlaps? m)
       
       ; did the missile intersect a line segement of the asteroid?
       (let ([x1 (get-field x m)]
             [x2 (get-field px m)]
             [y1 (get-field y m)]
             [y2 (get-field py m)])
         (send a intersects? x1 y1 x2 y2))))

;; ----------------------------------------------------

(define (collide-missiles)
  (for ([m missiles])
    (for ([a asteroids])
      (when (and (get-field alive m)
                 (test-missile-collision m a))
        (set! score (+ score (* (get-field size a) 100) 50))
        (r:play-sound boom-sound)
        (send a explode)
        (send m destroy)))))

;; ----------------------------------------------------

(define (draw-game-objs)
  (for ([asteroid asteroids])
    (send asteroid draw))
  (for ([missile missiles])
    (send missile draw))
  (send player draw))

;; ----------------------------------------------------

(define (draw-ui)
  (r:font r:tall-font)
  (r:text 1 1 score))

;; ----------------------------------------------------

(define (advance-game-objs)
  (send player advance)

  ; advance missiles and asteroids
  (for ([o missiles]) (send o advance))
  (for ([o asteroids]) (send o advance))

  ; perform collisions
  (collide-missiles)
  
  ; remove dead objects
  (set! missiles (filter (λ (o) (get-field alive o)) missiles))
  (set! asteroids (filter (λ (o) (get-field alive o)) asteroids)))

;; ----------------------------------------------------

(define (play-level)
  (r:cls)

  ; game objects and ui
  (draw-game-objs)
  (draw-ui)

  ; move objects
  (advance-game-objs)

  ; spawn anoter asteroid
  (when (spawn-timer)
    (spawn-asteroid))
  
  ; player controls
  (when (btn-shoot)
    (launch-missile))
  (when (r:btn-left)
    (send player turn -270))
  (when (r:btn-right)
    (send player turn 270))
  (when (r:btn-up)
    (send player apply-impulse 1))

  ; check for game quit
  (when (r:btn-quit)
    (r:quit)))

;; ----------------------------------------------------

(define (ready-player)
  (r:cls)

  ;
  (r:font r:tall-font)
  (r:text 1 1 (format "Ready level ~a" level))
  (r:text 1 11 (format "Press any button"))

  ;
  (r:wait)
  (r:goto play-level))

;; ----------------------------------------------------

(define (new-level)
  (set! level (+ level 1))

  ; spawn a single asteroid
  (spawn-asteroid)

  ; wait for the player to be ready
  (r:goto play-level))

;; ----------------------------------------------------

(define (new-game)
  (set! score 0)
  (set! asteroids null)
  (set! missiles null)
  (set! player (new game-obj%
                    [x (/ (r:width) 2)]
                    [y (/ (r:height) 2)]
                    [fx 0]
                    [fy -1]
                    [pts '((0 . -6)
                           (5 . 5)
                           (0 . 1)
                           (-5 . 5)
                           (0 . -6))])))

;; ----------------------------------------------------

(define (play)
  (r:run new-level 256 256 #:init new-game #:title "Shooter"))

;; ----------------------------------------------------

(module+ main
  (play))
