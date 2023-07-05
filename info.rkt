#lang info

(define collection "r-cade")
(define version "0.5.2")
(define pkg-authors '("massung@gmail.com"))
(define pkg-desc "R-cade game engine")
(define deps '("base" "csfml" "racket-doc" "scribble-lib"))
(define scribblings '(("scribblings/r-cade.scrbl" () ("Game development"))))
(define compile-omit-paths '("examples"))
