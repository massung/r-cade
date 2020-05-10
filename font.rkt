#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require racket/sequence)

;; ----------------------------------------------------

(provide (all-defined-out)
         (struct-out font-info))

;; ----------------------------------------------------

(struct font-info [width glyphs])

;; ----------------------------------------------------

(define (make-font width sprites)
  (font-info width (for/vector
                       ([sprite sprites]) sprite)))

;; ----------------------------------------------------

(define font (make-parameter #f))

;; ----------------------------------------------------

(define (font-width)
  (font-info-width (font)))

;; ----------------------------------------------------

(define (font-height)
  (+ (sequence-length (vector-ref (font-info-glyphs (font)) 0)) 1))

;; ----------------------------------------------------

(define (font-sprite char)
  (let ([n (char->integer char)])
    (and (<= 33 n 127)
         (vector-ref (font-info-glyphs (font)) (- n 33)))))

;; ----------------------------------------------------

(define basic-font
  (make-font 4 #((#x40 #x40 #x40 #x00 #x40 #x00)    ; !
                 (#xa0 #xa0 #x00 #x00 #x00 #x00)    ; "
                 (#xa0 #xe0 #xa0 #xe0 #xa0 #x00)    ; #
                 (#x60 #xc0 #x40 #x60 #xc0 #x00)    ; $
                 (#xa0 #x20 #x40 #x80 #xa0 #x00)    ; %
                 (#xc0 #xc0 #x80 #xe0 #xc0 #x00)    ; &
                 (#x40 #x40 #x00 #x00 #x00 #x00)    ; '
                 (#x20 #x40 #x40 #x40 #x20 #x00)    ; (
                 (#x80 #x40 #x40 #x40 #x80 #x00)    ; )
                 (#x00 #xa0 #x40 #xa0 #x00 #x00)    ; *
                 (#x00 #x40 #xe0 #x40 #x00 #x00)    ; +
                 (#x00 #x00 #x00 #x40 #x40 #x80)    ; ,
                 (#x00 #x00 #xe0 #x00 #x00 #x00)    ; -
                 (#x00 #x00 #x00 #x00 #x40 #x00)    ; .
                 (#x20 #x20 #x40 #x80 #x80 #x00)    ; /
                 (#x60 #xa0 #xa0 #xa0 #xc0 #x00)    ; 0
                 (#x40 #xc0 #x40 #x40 #xe0 #x00)    ; 1
                 (#xc0 #x20 #x40 #x80 #xe0 #x00)    ; 2
                 (#xc0 #x20 #x40 #x20 #xc0 #x00)    ; 3
                 (#xa0 #xa0 #xe0 #x20 #x20 #x00)    ; 4
                 (#xe0 #x80 #xc0 #x20 #xc0 #x00)    ; 5
                 (#x60 #x80 #xe0 #xa0 #xe0 #x00)    ; 6
                 (#xe0 #x20 #x40 #x80 #x80 #x00)    ; 7
                 (#xe0 #xa0 #xe0 #xa0 #xe0 #x00)    ; 8
                 (#xe0 #xa0 #xe0 #x20 #x20 #x00)    ; 9
                 (#x00 #x40 #x00 #x40 #x00 #x00)    ; :
                 (#x00 #x40 #x00 #x40 #x40 #x80)    ; ;
                 (#x20 #x40 #x80 #x40 #x20 #x00)    ; <
                 (#x00 #xe0 #x00 #xe0 #x00 #x00)    ; =
                 (#x80 #x40 #x20 #x40 #x80 #x00)    ; >
                 (#xc0 #x20 #x40 #x00 #x40 #x00)    ; ?
                 (#x00 #x60 #xa0 #x80 #x60 #x00)    ; @
                 (#x40 #xa0 #xe0 #xa0 #xa0 #x00)    ; A
                 (#xc0 #xa0 #xc0 #xa0 #xc0 #x00)    ; B
                 (#x40 #xa0 #x80 #xa0 #x40 #x00)    ; C
                 (#xc0 #xa0 #xa0 #xa0 #xc0 #x00)    ; D
                 (#xe0 #x80 #xc0 #x80 #xe0 #x00)    ; E
                 (#xe0 #x80 #xc0 #x80 #x80 #x00)    ; F
                 (#x60 #x80 #xa0 #xa0 #xc0 #x00)    ; G
                 (#xa0 #xa0 #xe0 #xa0 #xa0 #x00)    ; H
                 (#xe0 #x40 #x40 #x40 #xe0 #x00)    ; I
                 (#xe0 #x20 #x20 #xa0 #xc0 #x00)    ; J
                 (#xa0 #xa0 #xc0 #xa0 #xa0 #x00)    ; K
                 (#x80 #x80 #x80 #x80 #xe0 #x00)    ; L
                 (#xa0 #xe0 #xa0 #xa0 #xa0 #x00)    ; M
                 (#xa0 #xe0 #xe0 #xa0 #xa0 #x00)    ; N
                 (#x40 #xa0 #xa0 #xa0 #x40 #x00)    ; O
                 (#xc0 #xa0 #xc0 #x80 #x80 #x00)    ; P
                 (#x40 #xa0 #xa0 #xe0 #x60 #x00)    ; Q
                 (#xc0 #xa0 #xc0 #xa0 #xa0 #x00)    ; R
                 (#x60 #x80 #x40 #x20 #xc0 #x00)    ; S
                 (#xe0 #x40 #x40 #x40 #x40 #x00)    ; T
                 (#xa0 #xa0 #xa0 #xa0 #xe0 #x00)    ; U
                 (#xa0 #xa0 #xa0 #xa0 #x40 #x00)    ; V
                 (#xa0 #xa0 #xa0 #xe0 #xa0 #x00)    ; W
                 (#xa0 #xa0 #x40 #xa0 #xa0 #x00)    ; X
                 (#xa0 #xa0 #x40 #x40 #x40 #x00)    ; Y
                 (#xe0 #x20 #x40 #x80 #xe0 #x00)    ; Z
                 (#x60 #x40 #x40 #x40 #x60 #x00)    ; [
                 (#x80 #x80 #x40 #x20 #x20 #x00)    ; \
                 (#xc0 #x40 #x40 #x40 #xc0 #x00)    ; ]
                 (#x40 #xa0 #x00 #x00 #x00 #x00)    ; ^
                 (#x00 #x00 #x00 #x00 #xe0 #x00)    ; _
                 (#x80 #x40 #x00 #x00 #x00 #x00)    ; `
                 (#x00 #x60 #xa0 #xa0 #x60 #x00)    ; a
                 (#x80 #xc0 #xa0 #xa0 #xc0 #x00)    ; b
                 (#x00 #x60 #x80 #x80 #x60 #x00)    ; c
                 (#x20 #x60 #xa0 #xa0 #x60 #x00)    ; d
                 (#x00 #x60 #xe0 #x80 #x60 #x00)    ; e
                 (#x60 #x40 #xe0 #x40 #x40 #x00)    ; f
                 (#x00 #xc0 #xa0 #xe0 #x20 #xe0)    ; g
                 (#x80 #xc0 #xa0 #xa0 #xa0 #x00)    ; h
                 (#x40 #x00 #x40 #x40 #x40 #x00)    ; i
                 (#x40 #x00 #x40 #x40 #x40 #xc0)    ; j
                 (#x80 #xa0 #xa0 #xc0 #xa0 #x00)    ; k
                 (#x40 #x40 #x40 #x40 #x40 #x00)    ; l
                 (#x00 #xe0 #xe0 #xa0 #xa0 #x00)    ; m
                 (#x00 #xc0 #xa0 #xa0 #xa0 #x00)    ; n
                 (#x00 #x40 #xa0 #xa0 #x40 #x00)    ; o
                 (#x00 #xc0 #xa0 #xa0 #xc0 #x80)    ; p
                 (#x00 #x60 #xa0 #xa0 #x60 #x20)    ; q
                 (#x00 #x60 #x80 #x80 #x80 #x00)    ; r
                 (#x00 #x60 #xc0 #x60 #xc0 #x00)    ; s
                 (#x40 #xe0 #x40 #x40 #x60 #x00)    ; t
                 (#x00 #xa0 #xa0 #xa0 #xe0 #x00)    ; u
                 (#x00 #xa0 #xa0 #xa0 #x40 #x00)    ; v
                 (#x00 #xa0 #xa0 #xe0 #xe0 #x00)    ; w
                 (#x00 #xa0 #x40 #x40 #xa0 #x00)    ; x
                 (#x00 #xa0 #xa0 #xe0 #x20 #xc0)    ; y
                 (#x00 #xe0 #x60 #x80 #xe0 #x00)    ; z
                 (#x60 #x40 #xc0 #x40 #x60 #x00)    ; {
                 (#x40 #x40 #x40 #x40 #x40 #x00)    ; |
                 (#xc0 #x40 #x60 #x40 #xc0 #x00)    ; }
                 (#x00 #x20 #xe0 #x80 #x00 #x00)))) ; ~

;; ----------------------------------------------------

(define tall-font
  (make-font 6 #((#x20 #x20 #x20 #x20 #x20 #x00 #x20 #x00)    ; !
                 (#x50 #x50 #x00 #x00 #x00 #x00 #x00 #x00)    ; "
                 (#x00 #x50 #xf8 #x50 #xf8 #x50 #x00 #x00)    ; #
                 (#x20 #x78 #xa0 #x70 #x28 #xf0 #x20 #x00)    ; $ 
                 (#xc8 #xc8 #x10 #x20 #x40 #x98 #x98 #x00)    ; %
                 (#x40 #xa0 #xa0 #x40 #xa8 #x90 #x68 #x00)    ; &
                 (#x20 #x20 #x00 #x00 #x00 #x00 #x00 #x00)    ; '
                 (#x30 #x40 #x80 #x80 #x80 #x40 #x30 #x00)    ; (
                 (#x60 #x10 #x08 #x08 #x08 #x10 #x60 #x00)    ; )
                 (#x00 #x20 #xa8 #x70 #xa8 #x20 #x00 #x00)    ; *
                 (#x00 #x20 #x20 #xf8 #x20 #x20 #x00 #x00)    ; +
                 (#x00 #x00 #x00 #x00 #x00 #x20 #x20 #x40)    ; ,
                 (#x00 #x00 #x00 #xf8 #x00 #x00 #x00 #x00)    ; -
                 (#x00 #x00 #x00 #x00 #x00 #x60 #x60 #x00)    ; .
                 (#x08 #x08 #x10 #x20 #x40 #x80 #x80 #x00)    ; /
                 (#x70 #x88 #x88 #xa8 #x88 #x88 #x70 #x00)    ; 0
                 (#x20 #x60 #x20 #x20 #x20 #x20 #xf8 #x00)    ; 1
                 (#x70 #x88 #x08 #x10 #x20 #x40 #xf8 #x00)    ; 2
                 (#x70 #x88 #x08 #x30 #x08 #x88 #x70 #x00)    ; 3
                 (#x88 #x88 #x88 #xf8 #x08 #x08 #x08 #x00)    ; 4
                 (#xf8 #x80 #x80 #xf0 #x08 #x08 #xf0 #x00)    ; 5
                 (#x70 #x88 #x80 #xf0 #x88 #x88 #x70 #x00)    ; 6
                 (#xf8 #x08 #x08 #x08 #x08 #x08 #x08 #x00)    ; 7
                 (#x70 #x88 #x88 #x70 #x88 #x88 #x70 #x00)    ; 8
                 (#x78 #x88 #x88 #x78 #x08 #x08 #x08 #x00)    ; 9
                 (#x00 #x00 #x60 #x60 #x00 #x60 #x60 #x00)    ; :
                 (#x00 #x00 #x60 #x60 #x00 #x60 #x20 #x40)    ; ;
                 (#x10 #x20 #x40 #x80 #x40 #x20 #x10 #x00)    ; <
                 (#x00 #x00 #xf8 #x00 #xf8 #x00 #x00 #x00)    ; =
                 (#x40 #x20 #x10 #x08 #x10 #x20 #x40 #x00)    ; >
                 (#x70 #x88 #x08 #x10 #x20 #x00 #x20 #x00)    ; ?
                 (#x70 #x88 #xb8 #xa8 #xb8 #x80 #x70 #x00)    ; @
                 (#x70 #x88 #x88 #xf8 #x88 #x88 #x88 #x00)    ; A
                 (#xf0 #x88 #x88 #xf0 #x88 #x88 #xf0 #x00)    ; B
                 (#x70 #x88 #x80 #x80 #x80 #x88 #x70 #x00)    ; C
                 (#xf0 #x88 #x88 #x88 #x88 #x88 #xf0 #x00)    ; D
                 (#xf8 #x80 #x80 #xe0 #x80 #x80 #xf8 #x00)    ; E
                 (#xf8 #x80 #x80 #xe0 #x80 #x80 #x80 #x00)    ; F
                 (#x70 #x88 #x80 #xb8 #x88 #x88 #x70 #x00)    ; G
                 (#x88 #x88 #x88 #xf8 #x88 #x88 #x88 #x00)    ; H
                 (#xf8 #x20 #x20 #x20 #x20 #x20 #xf8 #x00)    ; I
                 (#xf8 #x08 #x08 #x08 #x08 #x88 #x70 #x00)    ; J
                 (#x88 #x90 #xa0 #xc0 #xa0 #x90 #x88 #x00)    ; K
                 (#x80 #x80 #x80 #x80 #x80 #x80 #xf8 #x00)    ; L
                 (#x88 #xd8 #xa8 #x88 #x88 #x88 #x88 #x00)    ; M
                 (#x88 #x88 #xc8 #xa8 #x98 #x88 #x88 #x00)    ; N
                 (#x70 #x88 #x88 #x88 #x88 #x88 #x70 #x00)    ; O
                 (#xf0 #x88 #x88 #xf0 #x80 #x80 #x80 #x00)    ; P
                 (#x70 #x88 #x88 #x88 #xa8 #x98 #x78 #x00)    ; Q
                 (#xf0 #x88 #x88 #xf0 #x88 #x88 #x88 #x00)    ; R
                 (#x70 #x88 #x80 #x70 #x08 #x88 #x70 #x00)    ; S
                 (#xf8 #x20 #x20 #x20 #x20 #x20 #x20 #x00)    ; T
                 (#x88 #x88 #x88 #x88 #x88 #x88 #x70 #x00)    ; U
                 (#x88 #x88 #x88 #x50 #x50 #x50 #x20 #x00)    ; V
                 (#x88 #x88 #x88 #x88 #xa8 #xd8 #x88 #x00)    ; W
                 (#x88 #x88 #x50 #x20 #x50 #x88 #x88 #x00)    ; X
                 (#x88 #x88 #x88 #x78 #x08 #x08 #xf0 #x00)    ; Y
                 (#xf8 #x08 #x10 #x20 #x40 #x80 #xf8 #x00)    ; Z
                 (#xf0 #x80 #x80 #x80 #x80 #x80 #xf0 #x00)    ; [
                 (#x80 #x80 #x40 #x20 #x10 #x08 #x08 #x00)    ; \
                 (#xf8 #x08 #x08 #x08 #x08 #x08 #xf8 #x00)    ; ]
                 (#x20 #x50 #x00 #x00 #x00 #x00 #x00 #x00)    ; ^
                 (#x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x00)    ; _
                 (#x40 #x20 #x10 #x00 #x00 #x00 #x00 #x00)    ; `
                 (#x00 #x00 #x78 #x88 #x88 #x98 #x68 #x00)    ; a
                 (#x80 #x80 #xf0 #x88 #x88 #x88 #xf0 #x00)    ; b
                 (#x00 #x00 #x70 #x88 #x80 #x88 #x70 #x00)    ; c
                 (#x08 #x08 #x78 #x88 #x88 #x88 #x78 #x00)    ; d
                 (#x00 #x00 #x70 #x88 #xf8 #x80 #x78 #x00)    ; e
                 (#x70 #x88 #x80 #xf0 #x80 #x80 #x80 #x00)    ; f
                 (#x00 #x00 #x78 #x88 #x88 #x78 #x08 #x70)    ; g
                 (#x80 #x80 #xf0 #x88 #x88 #x88 #x88 #x00)    ; h
                 (#x00 #x20 #x00 #x20 #x20 #x20 #x20 #x00)    ; i
                 (#x00 #x08 #x00 #x08 #x08 #x08 #x88 #x70)    ; j
                 (#x80 #x80 #x98 #xa0 #xc0 #xa0 #x98 #x00)    ; k
                 (#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x00)    ; l
                 (#x00 #x00 #xf0 #xa8 #xa8 #x88 #x88 #x00)    ; m
                 (#x00 #x00 #xf0 #x88 #x88 #x88 #x88 #x00)    ; n
                 (#x00 #x00 #x70 #x88 #x88 #x88 #x70 #x00)    ; o
                 (#x00 #x00 #xf0 #x88 #x88 #xf0 #x80 #x80)    ; p
                 (#x00 #x00 #x78 #x88 #x88 #x78 #x08 #x08)    ; q
                 (#x00 #x00 #x70 #x88 #x80 #x80 #x80 #x00)    ; r
                 (#x00 #x00 #x78 #x80 #x70 #x08 #xf0 #x00)    ; s
                 (#x00 #x40 #x78 #x40 #x40 #x40 #x38 #x00)    ; t
                 (#x00 #x00 #x88 #x88 #x88 #x88 #x70 #x00)    ; u
                 (#x00 #x00 #x88 #x88 #x50 #x50 #x20 #x00)    ; v
                 (#x00 #x00 #x88 #x88 #xa8 #xa8 #x50 #x00)    ; w
                 (#x00 #x00 #x88 #x50 #x20 #x50 #x88 #x00)    ; x
                 (#x00 #x00 #x88 #x88 #x88 #x78 #x08 #x70)    ; y
                 (#x00 #x00 #xf8 #x10 #x20 #x40 #xf8 #x00)    ; z
                 (#x70 #x40 #x40 #x80 #x40 #x40 #x70 #x00)    ; {
                 (#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x00)    ; |
                 (#x70 #x10 #x10 #x08 #x10 #x10 #x70 #x00)    ; }
                 (#x00 #x10 #x50 #xa0 #x80 #x00 #x00 #x00)))) ; ~

;; ----------------------------------------------------

(define wide-font
  (make-font 8 #((#x30 #x30 #x30 #x30 #x00 #x30 #x00 #x00)    ; !
                 (#xd8 #xd8 #x00 #x00 #x00 #x00 #x00 #x00)    ; "
                 (#x6c #x6c #xfe #x6c #xfe #x6c #x6c #x00)    ; #
                 (#x10 #x7e #x90 #x7c #x12 #xfc #x10 #x00)    ; $
                 (#xc0 #xcc #x18 #x30 #x60 #xcc #x0c #x00)    ; %
                 (#x70 #xd8 #xd8 #x70 #xda #xdc #x76 #x00)    ; &
                 (#x18 #x30 #x60 #x00 #x00 #x00 #x00 #x00)    ; '
                 (#x18 #x30 #x60 #x60 #x60 #x30 #x18 #x00)    ; (
                 (#x60 #x30 #x18 #x18 #x18 #x30 #x60 #x00)    ; )
                 (#x00 #x30 #xfc #x78 #xfc #x30 #x00 #x00)    ; *
                 (#x00 #x30 #x30 #xfc #x30 #x30 #x00 #x00)    ; +
                 (#x00 #x00 #x00 #x00 #x00 #x30 #x30 #x60)    ; ,
                 (#x00 #x00 #x00 #xfc #x00 #x00 #x00 #x00)    ; -
                 (#x00 #x00 #x00 #x00 #x00 #x30 #x30 #x00)    ; .
                 (#x00 #x0c #x18 #x30 #x60 #xc0 #x00 #x00)    ; /
                 (#x78 #xcc #xdc #xfc #xec #xcc #x78 #x00)    ; 0
                 (#x30 #x70 #x30 #x30 #x30 #x30 #xfc #x00)    ; 1
                 (#x78 #xcc #x0c #x18 #x30 #x60 #xfc #x00)    ; 2
                 (#x78 #xcc #x0c #x38 #x0c #xcc #x78 #x00)    ; 3
                 (#x18 #x38 #x78 #xd8 #xfc #x18 #x18 #x00)    ; 4
                 (#xfc #xc0 #xf8 #x0c #x0c #xcc #x78 #x00)    ; 5
                 (#x38 #x60 #xc0 #xf8 #xcc #xcc #x78 #x00)    ; 6
                 (#xfc #x0c #x18 #x30 #x60 #x60 #x60 #x00)    ; 7
                 (#x78 #xcc #xcc #x78 #xcc #xcc #x78 #x00)    ; 8
                 (#x78 #xcc #xcc #x7c #x0c #x18 #x70 #x00)    ; 9
                 (#x00 #x00 #x30 #x30 #x00 #x30 #x30 #x00)    ; :
                 (#x00 #x00 #x30 #x30 #x00 #x30 #x30 #x60)    ; ;
                 (#x18 #x30 #x60 #xc0 #x60 #x30 #x18 #x00)    ; <
                 (#x00 #x00 #xfc #x00 #xfc #x00 #x00 #x00)    ; =
                 (#x60 #x30 #x18 #x0c #x18 #x30 #x60 #x00)    ; >
                 (#x78 #xcc #x18 #x30 #x30 #x00 #x30 #x00)    ; ?
                 (#x78 #xcc #xdc #xd4 #xdc #xc0 #x78 #x00)    ; @
                 (#x78 #xcc #xcc #xfc #xcc #xcc #xcc #x00)    ; A
                 (#xf8 #xcc #xcc #xf8 #xcc #xcc #xf8 #x00)    ; B
                 (#x78 #xcc #xc0 #xc0 #xc0 #xcc #x78 #x00)    ; C
                 (#xf8 #xcc #xc6 #xc6 #xc6 #xcc #xf8 #x00)    ; D
                 (#xfc #xc0 #xc0 #xf8 #xc0 #xc0 #xfc #x00)    ; E
                 (#xfc #xc0 #xc0 #xf8 #xc0 #xc0 #xc0 #x00)    ; F
                 (#x78 #xcc #xc0 #xdc #xcc #xcc #x78 #x00)    ; G
                 (#xcc #xcc #xcc #xfc #xcc #xcc #xcc #x00)    ; H
                 (#xfc #x30 #x30 #x30 #x30 #x30 #xfc #x00)    ; I
                 (#x7c #x18 #x18 #x18 #x18 #xd8 #x70 #x00)    ; J
                 (#xcc #xd8 #xf0 #xe0 #xf0 #xd8 #xcc #x00)    ; K
                 (#xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xfc #x00)    ; L
                 (#xc6 #xee #xfe #xd6 #xc6 #xc6 #xc6 #x00)    ; M
                 (#xcc #xec #xfc #xdc #xcc #xcc #xcc #x00)    ; N
                 (#x78 #xcc #xcc #xcc #xcc #xcc #x78 #x00)    ; O
                 (#xf8 #xcc #xcc #xf8 #xc0 #xc0 #xc0 #x00)    ; P
                 (#x78 #xcc #xcc #xcc #xd4 #xd8 #x6c #x00)    ; Q
                 (#xf8 #xcc #xcc #xf8 #xd8 #xcc #xcc #x00)    ; R
                 (#x78 #xcc #xc0 #x78 #x0c #xcc #x78 #x00)    ; S
                 (#xfc #x30 #x30 #x30 #x30 #x30 #x30 #x00)    ; T
                 (#xcc #xcc #xcc #xcc #xcc #xcc #x78 #x00)    ; U
                 (#xcc #xcc #xcc #xcc #xcc #x78 #x30 #x00)    ; V
                 (#xc6 #xc6 #xd6 #xd6 #xfe #xee #xc6 #x00)    ; W
                 (#xcc #xcc #x78 #x30 #x78 #xcc #xcc #x00)    ; X
                 (#xcc #xcc #xcc #x78 #x30 #x30 #x30 #x00)    ; Y
                 (#xfc #x0c #x18 #x30 #x60 #xc0 #xfc #x00)    ; Z
                 (#x7c #x60 #x60 #x60 #x60 #x60 #x7c #x00)    ; [
                 (#x00 #xc0 #x60 #x30 #x18 #x0c #x00 #x00)    ; \
                 (#x7c #x0c #x0c #x0c #x0c #x0c #x7c #x00)    ; ]
                 (#x30 #x78 #xcc #x84 #x00 #x00 #x00 #x00)    ; ^
                 (#x00 #x00 #x00 #x00 #x00 #x00 #xfe #x00)    ; _
                 (#x60 #x30 #x18 #x00 #x00 #x00 #x00 #x00)    ; `
                 (#x00 #x00 #x78 #x0c #x7c #xcc #x7c #x00)    ; a
                 (#xc0 #xc0 #xf8 #xcc #xcc #xcc #xf8 #x00)    ; b
                 (#x00 #x00 #x78 #xcc #xc0 #xcc #x78 #x00)    ; c
                 (#x0c #x0c #x7c #xcc #xcc #xcc #x7c #x00)    ; d
                 (#x00 #x00 #x78 #xcc #xfc #xc0 #x78 #x00)    ; e
                 (#x38 #x60 #x60 #xf8 #x60 #x60 #x60 #x00)    ; f
                 (#x00 #x00 #x7c #xcc #xcc #x7c #x0c #x78)    ; g
                 (#xc0 #xc0 #xf8 #xcc #xcc #xcc #xcc #x00)    ; h
                 (#x30 #x00 #x70 #x30 #x30 #x30 #x78 #x00)    ; i
                 (#x30 #x00 #x70 #x30 #x30 #x30 #x30 #xe0)    ; j
                 (#xc0 #xc0 #xcc #xd8 #xf0 #xd8 #xcc #x00)    ; k
                 (#x70 #x30 #x30 #x30 #x30 #x30 #x78 #x00)    ; l
                 (#x00 #x00 #x6c #xd6 #xd6 #xc6 #xc6 #x00)    ; m
                 (#x00 #x00 #xf8 #xcc #xcc #xcc #xcc #x00)    ; n
                 (#x00 #x00 #x78 #xcc #xcc #xcc #x78 #x00)    ; o
                 (#x00 #x00 #xf8 #xcc #xcc #xf8 #xc0 #xc0)    ; p
                 (#x00 #x00 #x7c #xcc #xcc #x7c #x0c #x0e)    ; q
                 (#x00 #x00 #xd8 #xec #xc0 #xc0 #xc0 #x00)    ; r
                 (#x00 #x00 #x7c #x80 #x78 #x04 #xf8 #x00)    ; s
                 (#x60 #x60 #xf8 #x60 #x60 #x60 #x38 #x00)    ; t
                 (#x00 #x00 #xcc #xcc #xcc #xcc #x7c #x00)    ; u
                 (#x00 #x00 #xcc #xcc #xcc #x78 #x30 #x00)    ; v
                 (#x00 #x00 #xc6 #xd6 #xd6 #xfe #x6c #x00)    ; w
                 (#x00 #x00 #xcc #x78 #x30 #x78 #xcc #x00)    ; x
                 (#x00 #x00 #xcc #xcc #xcc #x7c #x0c #x78)    ; y
                 (#x00 #x00 #xfc #x18 #x30 #x60 #xfc #x00)    ; z
                 (#x1c #x30 #x30 #x60 #x30 #x30 #x1c #x00)    ; {
                 (#x30 #x30 #x30 #x30 #x30 #x30 #x30 #x00)    ; |
                 (#x70 #x18 #x18 #x0c #x18 #x18 #x70 #x00)    ; }
                 (#x00 #x64 #xb4 #x98 #x00 #x00 #x00 #x00)))) ; ~
