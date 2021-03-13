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

                 ; constants
                 "const float BLEND = 0.7;"
                 "const float SIGMA = 7.0;"
                 "const int MSIZE = 7;"
                 "const int KSIZE = (MSIZE-1) / 2;"

                 ; normalize
                 "float normpdf(float x) {"
                 "    return 0.39894*exp(-0.5*x*x/(SIGMA*SIGMA))/SIGMA;"
                 "}"

                 ; uv coordinate of pixel + offset
                 "vec2 uv(vec2 pixel, int x, int y) {"
                 "    vec2 p = pixel + (vec2(float(x), float(y)) / scale);"
                 "    return p / resolution;"
                 "}"

                 ; texture lookup
                 "vec3 tex(vec2 pixel, int x, int y) {"
                 "    return texture2D(texture, uv(pixel, x, y)).rgb;"
                 "}"

                 ; rgb scanlines
                 "vec4 scanline(float y, vec3 color) {"
                 "    float s = y - (3.0 * floor(y / 3.0));"
                 "    vec4 c = vec4(color, 1.0);"

                 ; no scanlines if nonot enough scaling
                 "    if (scale < 4.0) return c;"

                 ; b, g, or r scanline?
                 "    if (s < 1.0) return c * vec4(BLEND, BLEND, 1.0, 1.0);"
                 "    if (s < 2.0) return c * vec4(BLEND, 1.0, BLEND, 1.0);"
                 "    return c * vec4(1.0, BLEND, BLEND, 1.0);"
                 "}"

                 ; entry point
                 "void main() {"
                 "    vec2 pixel = gl_TexCoord[0].xy * resolution;"

                 ; create the 1D kernel
                 "    float kernel[MSIZE];"
                 "    for (int j = 0;j < KSIZE;j++) {"
                 "        kernel[KSIZE+j] = kernel[KSIZE-j] = normpdf(float(j));"
                 "    }"

                 ; calculate the normalization factor
                 "    float z = 0.0;"
                 "    for (int i=0;i < MSIZE;i++) {"
                 "        z += kernel[i];"
                 "    }"

                 ; perform blur
                 ;"    vec3 color = tex(pixel, 0, 0);"
                 "    vec3 color = vec3(0.0, 0.0, 0.0);"
                 "    for (int i=-KSIZE;i <= KSIZE;i++) {"
                 "        for (int j=-KSIZE;j <= KSIZE;j++) {"
                 "            color += kernel[KSIZE+i] * kernel[KSIZE+j] * tex(pixel, i, j);"
                 "        }"
                 "    }"
                 
                 "    gl_FragColor = scanline(pixel.y * scale, color / (z*z));"
                 "}"))

;; ----------------------------------------------------

(define vertex-shader basic-vertex-shader)
(define fragment-shader crt-fragment-shader)
