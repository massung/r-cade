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

                 ; texel size
                 "float BLEND = 0.7;"

                 ; how close to the edge is a pixel?
                 "vec4 scanline(float y, vec3 color) {"
                 "    float s = y - (3.0 * floor(y / 3.0));"
                 ""
                 "    vec4 c = vec4(color, 1.0);"

                 ; b, g, or r scanline?
                 "    if (s < 1.0) return c * vec4(BLEND, BLEND, 1.0, 1.0);"
                 "    if (s < 2.0) return c * vec4(BLEND, 1.0, BLEND, 1.0);"
                 ""
                 "    return c * vec4(1.0, BLEND, BLEND, 1.0);"
                 "}"
                 
                 "void main() {"
                 "    vec2 uv = gl_TexCoord[0].xy;"
                 "    vec3 color = texture2D(texture, uv).rgb;"
                 
                 "    gl_FragColor = scanline(uv.y * resolution.y * scale, color);"
                 "}"))

;; ----------------------------------------------------

(define vertex-shader basic-vertex-shader)
(define fragment-shader crt-fragment-shader)
