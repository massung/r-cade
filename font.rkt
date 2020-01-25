#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(provide (all-defined-out))

;; ----------------------------------------------------

(define font
  (vector (list #x40 #x40 #x40 #x00 #x40 #x00)    ; !
          (list #xa0 #xa0 #x00 #x00 #x00 #x00)    ; "
          (list #xa0 #xe0 #xa0 #xe0 #xa0 #x00)    ; #
          (list #x60 #xc0 #x40 #x60 #xc0 #x00)    ; $
          (list #xa0 #x20 #x40 #x80 #xa0 #x00)    ; %
          (list #xc0 #xc0 #x80 #xe0 #xc0 #x00)    ; &
          (list #x40 #x40 #x00 #x00 #x00 #x00)    ; '
          (list #x20 #x40 #x40 #x40 #x20 #x00)    ; (
          (list #x80 #x40 #x40 #x40 #x80 #x00)    ; )
          (list #x00 #xa0 #x40 #xa0 #x00 #x00)    ; *
          (list #x00 #x40 #xe0 #x40 #x00 #x00)    ; +
          (list #x00 #x00 #x00 #x40 #x40 #x80)    ; ,
          (list #x00 #x00 #xe0 #x00 #x00 #x00)    ; -
          (list #x00 #x00 #x00 #x00 #x40 #x00)    ; .
          (list #x20 #x20 #x40 #x80 #x80 #x00)    ; /
          (list #x60 #xa0 #xa0 #xa0 #xc0 #x00)    ; 0
          (list #x40 #xc0 #x40 #x40 #xe0 #x00)    ; 1
          (list #xc0 #x20 #x40 #x80 #xe0 #x00)    ; 2
          (list #xc0 #x20 #x40 #x20 #xc0 #x00)    ; 3
          (list #xa0 #xa0 #xe0 #x20 #x20 #x00)    ; 4
          (list #xe0 #x80 #xc0 #x20 #xc0 #x00)    ; 5
          (list #x60 #x80 #xe0 #xa0 #xe0 #x00)    ; 6
          (list #xe0 #x20 #x40 #x80 #x80 #x00)    ; 7
          (list #xe0 #xa0 #xe0 #xa0 #xe0 #x00)    ; 8
          (list #xe0 #xa0 #xe0 #x20 #x20 #x00)    ; 9
          (list #x00 #x40 #x00 #x40 #x00 #x00)    ; :
          (list #x00 #x40 #x00 #x40 #x40 #x80)    ; ;
          (list #x20 #x40 #x80 #x40 #x20 #x00)    ; <
          (list #x00 #xe0 #x00 #xe0 #x00 #x00)    ; =
          (list #x80 #x40 #x20 #x40 #x80 #x00)    ; >
          (list #xc0 #x20 #x40 #x00 #x40 #x00)    ; ?
          (list #x00 #x60 #xa0 #x80 #x60 #x00)    ; @
          (list #x40 #xa0 #xe0 #xa0 #xa0 #x00)    ; A
          (list #xc0 #xa0 #xc0 #xa0 #xc0 #x00)    ; B
          (list #x40 #xa0 #x80 #xa0 #x40 #x00)    ; C
          (list #xc0 #xa0 #xa0 #xa0 #xc0 #x00)    ; D
          (list #xe0 #x80 #xc0 #x80 #xe0 #x00)    ; E
          (list #xe0 #x80 #xc0 #x80 #x80 #x00)    ; F
          (list #x60 #x80 #xa0 #xa0 #xc0 #x00)    ; G
          (list #xa0 #xa0 #xe0 #xa0 #xa0 #x00)    ; H
          (list #xe0 #x40 #x40 #x40 #xe0 #x00)    ; I
          (list #xe0 #x20 #x20 #xa0 #xc0 #x00)    ; J
          (list #xa0 #xa0 #xc0 #xa0 #xa0 #x00)    ; K
          (list #x80 #x80 #x80 #x80 #xe0 #x00)    ; L
          (list #xa0 #xe0 #xa0 #xa0 #xa0 #x00)    ; M
          (list #xa0 #xe0 #xe0 #xa0 #xa0 #x00)    ; N
          (list #x40 #xa0 #xa0 #xa0 #x40 #x00)    ; O
          (list #xc0 #xa0 #xc0 #x80 #x80 #x00)    ; P
          (list #x40 #xa0 #xa0 #xe0 #x60 #x00)    ; Q
          (list #xc0 #xa0 #xc0 #xa0 #xa0 #x00)    ; R
          (list #x60 #x80 #x40 #x20 #xc0 #x00)    ; S
          (list #xe0 #x40 #x40 #x40 #x40 #x00)    ; T
          (list #xa0 #xa0 #xa0 #xa0 #xe0 #x00)    ; U
          (list #xa0 #xa0 #xa0 #xa0 #x40 #x00)    ; V
          (list #xa0 #xa0 #xa0 #xe0 #xa0 #x00)    ; W
          (list #xa0 #xa0 #x40 #xa0 #xa0 #x00)    ; X
          (list #xa0 #xa0 #x40 #x40 #x40 #x00)    ; Y
          (list #xe0 #x20 #x40 #x80 #xe0 #x00)    ; Z
          (list #x60 #x40 #x40 #x40 #x60 #x00)    ; [
          (list #x80 #x80 #x40 #x20 #x20 #x00)    ; \
          (list #xc0 #x40 #x40 #x40 #xc0 #x00)    ; ]
          (list #x40 #xa0 #x00 #x00 #x00 #x00)    ; ^
          (list #x00 #x00 #x00 #x00 #xe0 #x00)    ; _
          (list #x80 #x40 #x00 #x00 #x00 #x00)    ; `
          (list #x00 #x60 #xa0 #xa0 #x60 #x00)    ; a
          (list #x80 #xc0 #xa0 #xa0 #xc0 #x00)    ; b
          (list #x00 #x60 #x80 #x80 #x60 #x00)    ; c
          (list #x20 #x60 #xa0 #xa0 #x60 #x00)    ; d
          (list #x00 #x60 #xe0 #x80 #x60 #x00)    ; e
          (list #x60 #x40 #xe0 #x40 #x40 #x00)    ; f
          (list #x00 #xc0 #xa0 #xe0 #x20 #xe0)    ; g
          (list #x80 #xc0 #xa0 #xa0 #xa0 #x00)    ; h
          (list #x40 #x00 #x40 #x40 #x40 #x00)    ; i
          (list #x40 #x00 #x40 #x40 #x40 #xc0)    ; j
          (list #x80 #xa0 #xa0 #xc0 #xa0 #x00)    ; k
          (list #x40 #x40 #x40 #x40 #x40 #x00)    ; l
          (list #x00 #xe0 #xe0 #xa0 #xa0 #x00)    ; m
          (list #x00 #xc0 #xa0 #xa0 #xa0 #x00)    ; n
          (list #x00 #x40 #xa0 #xa0 #x40 #x00)    ; o
          (list #x00 #xc0 #xa0 #xa0 #xc0 #x80)    ; p
          (list #x00 #x60 #xa0 #xa0 #x60 #x20)    ; q
          (list #x00 #x60 #x80 #x80 #x80 #x00)    ; r
          (list #x00 #x60 #xc0 #x60 #xc0 #x00)    ; s
          (list #x40 #xe0 #x40 #x40 #x60 #x00)    ; t
          (list #x00 #xa0 #xa0 #xa0 #xe0 #x00)    ; u
          (list #x00 #xa0 #xa0 #xa0 #x40 #x00)    ; v
          (list #x00 #xa0 #xa0 #xe0 #xe0 #x00)    ; w
          (list #x00 #xa0 #x40 #x40 #xa0 #x00)    ; x
          (list #x00 #xa0 #xa0 #xe0 #x20 #xc0)    ; y
          (list #x00 #xe0 #x60 #x80 #xe0 #x00)    ; z
          (list #x60 #x40 #xc0 #x40 #x60 #x00)    ; {
          (list #x40 #x40 #x40 #x40 #x40 #x00)    ; |
          (list #xc0 #x40 #x60 #x40 #xc0 #x00)    ; }
          (list #x00 #x20 #xe0 #x80 #x00 #x00)))  ; ~
