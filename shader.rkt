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
is a derivative of several other shaders, most of which were
found here:

  https://github.com/libretro/glsl-shaders

A hearty thanks goes out to the CRT shader emulation community
for their hard work!!

|#

;; ----------------------------------------------------

(define crt-shader
  (string-append "void main() {"
                 "    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;"
                 "    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;"
                 "}"))

;; ----------------------------------------------------

(define scanline-shader
  (string-append "uniform sampler2D texture;"
                 "uniform vec2 resolution;"

                 ; scanline settings
                 "const float SCANLINE = 0.5;"
                 "const float SQRT_2PI = 2.506628;"

                 ; luminance of a color
                 "float lum(vec3 color) {"
                 "    return color.r * 0.2126 + color.g * 0.7152 + color.b * 0.0722;"
                 "}"

                 ; normal distribution
                 "float dist(float x) {"
                 "    return exp(-0.5 * x * x);"; * SQRT_2PI;"
                 "}"

                 ; returns the phosphor color at the pixel
                 "vec3 phosphor() {"
                 "    vec2 uv = gl_TexCoord[0].xy;"
                 "    vec3 color = texture2D(texture, uv).rgb;"

                 ; normal distribution of b, g, r across pixel
                 "    float x = mod(uv.x * resolution.x * 3.0, 3.0);"
                 "    float m = lum(color);"
                 "    float b = dist(x - 0.0);"
                 "    float g = dist(x - 1.0);"
                 "    float r = dist(x - 2.0);"

                 ; phosphor channel scaled by luminance of pixel
                 "    return mix(color, vec3(r, g, b) * m, 0.4);"
                 "}"

                 ; pixel color vs. scanline
                 "vec3 scanline() {"
                 "    float y = gl_TexCoord[0].y * resolution.y;"
                 "    float b = floor(y);"
                 "    float w = 1.0;"
                 "    float x = y - b;"

                 ; exponential decay color when close to scanline
                 "    if (x > SCANLINE) {"
                 "        w = mix(1.0, 0.0, (x - SCANLINE) / (1.0 - SCANLINE));"
                 "    }"

                 ; combine horizontal and vertical scanline
                 "    return vec3(1.0, 1.0, 1.0) * w;"
                 "}"
                 
                 "void main() {"
                 "    gl_FragColor = vec4(phosphor() * scanline(), 1.0);"
                 "}"))

;; ----------------------------------------------------

(define vertex-shader crt-shader)
(define fragment-shader scanline-shader)
