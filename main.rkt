#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require ffi/unsafe)
(require csfml)

;; ----------------------------------------------------

(require "video.rkt")
(require "game.rkt")
(require "input.rkt")
(require "palette.rkt")
(require "font.rkt")
(require "draw.rkt")
(require "voice.rkt")
(require "sound.rkt")
(require "audio.rkt")
(require "music.rkt")

;; ----------------------------------------------------

(provide

 ; game
 define-action
 define-timer
 run
 quit
 goto
 wait
 sync
 frame
 frametime
 gametime
 width
 height

 ; input
 btn-any
 btn-start
 btn-select
 btn-quit
 btn-z
 btn-x
 btn-up
 btn-down
 btn-right
 btn-left
 btn-mouse
 mouse-x
 mouse-y
 hide-mouse
 show-mouse

 ; draw
 cls
 color
 set-color!
 draw
 draw-ex
 text
 line
 rect
 circle

 ; font
 font
 font-width
 font-height
 make-font
 basic-font
 tall-font
 wide-font

 ; voice
 (struct-out voice)
 basic-voice
 sine-wave
 sawtooth-wave
 square-wave
 triangle-wave
 noise-wave
 synth
 envelope
 basic-envelope
 fade-in-envelope
 fade-out-envelope
 z-envelope
 s-envelope
 peak-envelope
 trough-envelope
 adsr-envelope

 ; audio
 music
 sound
 tone
 sweep
 basic-note
 play-sound
 stop-sound
 sound-volume
 music-volume
 play-music
 stop-music
 pause-music)
