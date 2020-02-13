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

The CRT-effect shaders below was taken (and modified) from
the Z_PI shaders, created by Greg Hogan:

  Copyright (C) 2017 Greg Hogan (SoltanGris42)

A hearty thanks goes out to the CRT shader emulation community
for their hard work!!

|#

(define lcd-shader
  (string-append "uniform sampler2D texture;"
                 "uniform vec2 textureSize;"
                 ""
                 "void main()"
                 "{"
                 "    vec2 invSize = 1.0 / textureSize.xy;"
                 "    vec2 texCoordInPixels = gl_TexCoord[0] * textureSize;"
                 "    vec2 centerCoord = floor(texCoordInPixels.xy) + vec2(0.5, 0.5);"
                 "    vec2 distFromCenter = abs(centerCoord - texCoordInPixels);"
                 ""
                 "    float Y = max(distFromCenter.x, distFromCenter.y);"
                 ""
                 "    Y = Y * Y;"
                 "    float YY = Y * Y;"
                 "    float YYY = YY * Y;"
                 ""
                 "    float lineWeight = YY - 2.7 * YYY;"
                 "    lineWeight = 1.0 - 14.0 * lineWeight;"
                 ""
                 "    vec3 color = texture2D(texture, invSize * centerCoord).rgb * lineWeight;"
                 "    color *= 0.6 + 0.4 * color.rgb;"
                 ""
                 "    gl_FragColor = vec4(color.rgb, 1.0);"
                 "}"))

;; ----------------------------------------------------

(define scanline-shader
  (string-append "uniform sampler2D texture;"
                 "uniform vec2 textureSize;"
                 ""
                 "void main()"
                 "{"
                 "    float whichMask = fract(gl_FragCoord.x * 0.5);"
                 "    float mask = 1.0 + float(whichMask < 0.5) * -0.3;"
                 ""
                 "    vec2 invSize = 1.0 / textureSize.xy;"
                 "    vec2 texCoordInPixels = gl_TexCoord[0] * textureSize;"
                 "    vec2 centerCoord = floor(texCoordInPixels.xy) + vec2(0.5, 0.5);"
                 "    vec2 distFromCenter = abs(centerCoord - texCoordInPixels);"
                 ""
                 "    float Y = distFromCenter.y * distFromCenter.y;"
                 "    float YY = Y * Y;"
                 "    float scanLineWeight = 1.2 - 6.0 * (Y - 2.05 * YY);"
                 "    float scanLineWeightB = 1.0 - 14.0 * (YY - 2.8 * YY * Y);"
                 ""
                 "    float tx = invSize.x * (centerCoord.x - 0.4 * distFromCenter.x);"
                 "    float ty = invSize.y * (centerCoord.y - 0.3 * distFromCenter.y);"
                 ""
                 "    vec2 tc = vec2(tx, ty);"
                 "    vec3 color = texture2D(texture, tc).rgb;"
                 ""
                 "    color.rgb *= 0.8 + 0.2 * color.rgb;"
                 "    color.rgb *= mix(scanLineWeight * mask, scanLineWeightB, dot(color.rgb, vec3(0.28)));"
                 ""
                 "    gl_FragColor = vec4(color.rgb, 1.0);"
                 "}"))

;; ----------------------------------------------------

(define crt-fragment-shader scanline-shader)
