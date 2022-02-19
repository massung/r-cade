#lang racket

#|

Racket Arcade (r-cade) - a simple game engine

Copyright (c) 2020 by Jeffrey Massung
All rights reserved.

|#

(require csfml)

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

;; ----------------------------------------------------

(define basic-vertex-shader
  (string-append "void main() {"
                 "    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;"
                 "    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;"
                 "}"))

;; ----------------------------------------------------

(define crt-fragment-shader
  (string-append "uniform sampler2D texture;"
                 "uniform vec2 resolution;"
                 "uniform float scale;"
                 "uniform float time;"

                 "float fmin = 0.25;"
                 
                 ; entry point
                 "void main() {"
                 "    vec2 st = gl_FragCoord.xy;"

                 ; color of pixel and scanline
                 "    vec3 pixel = texture2D(texture, gl_TexCoord[0].xy).rgb;"
                 "    vec4 scan = vec4(st.x, st.y, abs(sin(time)), 1.0);"

                 ; every 3rd pixel should be a scanline
                 "    float fmod = mod(scan.y, 3.0);"
                 "    float fstep = fmin + (1.0 - fmin) * fmod;"

                 ; alpha the color by the scanline
                 "    gl_FragColor = vec4(pixel.rgb, fstep);"
                 "}"))

;; ----------------------------------------------------

(define vertex-shader basic-vertex-shader)
(define fragment-shader crt-fragment-shader)
