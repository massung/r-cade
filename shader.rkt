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

This CRT-effect shader below was taken (and modified) from:

  https://filthypants.blogspot.com/2011/05/more-emulator-pixel-shaders-crt-updated.html

A hearty thanks goes out to the CRT shader emulation community
for their hard work!!

|#

(define crt-fragment-shader
  (string-append "precision highp float;"
                 "precision highp sampler2D;"
                 ""
                 "uniform sampler2D texture;"
                 "uniform vec2 textureSize;"
                 ""
                 "vec3 to_focus(float p)"
                 "{"
                 "    p = mod(p + 3.0, 3.0);"
                 "    "
                 "    if (p >= 2.0)"
                 "      return vec3(p - 2.0, 0.0, 3.0 - p);"
                 "    else if (p >= 1.0)"
                 "      return vec3(0.0, 2.0 - p, p - 1.0);"
                 "    else"
                 "      return vec3(1.0 - p, p, 0.0);"
                 "}"
                 ""
                 "void main()"
                 "{"
                 "    float y = mod(gl_TexCoord[0].y * textureSize.y, 1.0);"
                 "    float intensity = exp(-0.2 * y);"
                 ""
                 "    vec2 one_x = vec2(1.0 / (3.0 * textureSize.x), 0.0);"
                 ""
                 "    vec3 color = texture2D(texture, gl_TexCoord[0].xy - 0.0 * one_x).rgb;"
                 "    vec3 color_p = texture2D(texture, gl_TexCoord[0].xy - 1.0 * one_x).rgb;"
                 "    vec3 color_pp = texture2D(texture, gl_TexCoord[0].xy - 2.0 * one_x).rgb;"
                 ""
                 "    float x = 3.0 * gl_TexCoord[0].x * textureSize.x;"
                 ""
                 "    vec3 focus = to_focus(x - 0.0);"
                 "    vec3 focus_p = to_focus(x - 1.0);"
                 "    vec3 focus_pp = to_focus(x - 2.0);"
                 ""
                 "    vec3 result = "
                 "      0.8 * color * focus + "
                 "      0.6 * color_p * focus_p +"
                 "      0.3 * color_pp * focus_pp;"
                 ""
                 "    result = 2.3 * pow(result, vec3(1.4));"
                 ""
                 "    gl_FragColor = vec4(intensity * result, 1.0);"
                 "}"))
