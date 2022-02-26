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

    ; final output
    "out vec4 finalColor;"
    
    ; input values from raylib
    "uniform sampler2D texture0;"
    "uniform vec4 colDiffuse;"

    ; input values from r-cade
    "uniform vec2 res;"
    "uniform float time;"
    
    ; entry point
    "void main() {"
    "    vec3 pixel = texture(texture0, fragTexCoord).rgb;"
    
    ; every 3rd pixel should be a scanline
    "    float fmin = 0.50;"
    "    float fmod = mod(gl_FragCoord.y, 3.0);"
    "    float fstep = fmin + (1.0 - fmin) * fmod;"
    
    ; alpha the color by the scanline
    "    finalColor = vec4(pixel, fstep);"
    "}"))

;; ----------------------------------------------------

(define bloom-fragment-shader
  '("#version 330"

    ; type precision
    "precision mediump float;"

    ; from default vertex shader
    "in vec2 fragTexCoord;"
    "in vec4 fragColor;"
    
    ; final output
    "out vec4 finalColor;"

    ; input values from raylib
    "uniform sampler2D texture0;"
    "uniform vec4 colDiffuse;"

    ; input values from r-cade
    "uniform vec2 res;"
    "uniform float time;"

    ; shader constants
    "const float samples = 5.0;"
    "const float quality = 2.5;"

    ; entry point
    "void main() {"
    "    vec4 sum = vec3(0.0);"
    "    vec2 sizeFactor = vec2(1.0) / res * quality;"

    ; vram texture color
    "    vec3 pixel = texture(texture0, fragTexCoord);"

    ; sample range - ideally (samples-1)/2
    "    const int range = 2;"
    
    ; sample surrounding pixels
    "    for(int x = -range; x <= range; x++) {"
    "        for(int y = -range; y <= range; y++) {"
    "            sum += texture(texture0, fragTexCoord + vec2(x, y) * sizeFactor) * 2;"
    "        }"
    "    }"

    ; calculate final color
    "    finalColor = (sum / (samples*samples)) + pixel;"
    "}"
    ))

;; ----------------------------------------------------

(define fragment-shader (string-join scanline-fragment-shader "\n"))
