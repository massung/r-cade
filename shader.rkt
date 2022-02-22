#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

;; ----------------------------------------------------

(provide (all-defined-out))

;; ----------------------------------------------------

#|

The shader below has been a slow work in progress of mine, but
is based on work I learned from many other shaders, most of
found here:

  https://github.com/libretro/glsl-shaders

A hearty thanks goes out to the CRT shader emulation community
for their hard work!!

|#

(define scanline-fragment-shader
  '("#version 330"

    ; type precision
    "precision mediump float;"

    ; from default vertex shader
    "in vec2 fragTexCoord;"
    "in vec4 fragColor;"
    "out vec4 finalColor;"
    
    ; input values from raylib
    "uniform sampler2D texture0;"
    "uniform vec4 colDiffuse;"
    
    ; custom inputs
    "uniform float time;"
    
    ; entry point
    "void main() {"
    "    vec2 uv = vec2(fragTexCoord.x, -fragTexCoord.y);"
    "    vec3 pixel = texture(texture0, uv).rgb;"
    
    ; every 3rd pixel should be a scanline
    "    float fmin = 0.25;"
    "    float fmod = mod(gl_FragCoord.y, 3.0);"
    "    float fstep = fmin + (1.0 - fmin) * fmod;"
    
    ; alpha the color by the scanline
    "    finalColor = vec4(pixel, fstep);"
    "}"))

;; ----------------------------------------------------

(define fragment-shader (string-join scanline-fragment-shader "\n"))
