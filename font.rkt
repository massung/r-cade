#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(provide (all-defined-out))

;; ----------------------------------------------------

(define font (make-parameter #f))

;; ----------------------------------------------------

(define basic-font
  #((#x40 #x40 #x40 #x00 #x40 #x00)    ; !
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
    (#x00 #x20 #xe0 #x80 #x00 #x00)))  ; ~
