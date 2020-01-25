# Racket Cade (R-cade)

The `r-cade` package is a very simple game engine for the [Racket][racket] programming language with a few, specific goals in mind:

* Designed for kids; doing simple things is simple
* No external assets; all rendering, sound, and music is code
* Retro; it's intended to teach some low-level fundamentals

More on these later in the README. For now...

## Screenshots

Here are some of the example programs:

![][twinkle]
![][tetris]
![][breakout]

## Installation

Assuming you have [Racket][racket] downloaded and installed for your platform, you can install `r-cade` using `raco`:

```bash
$ raco pkg install r-cade
```

The only dependency `r-cade` has are [CSFML][csfml] bindings. The bindings for it can be found [here][csfml-package], but should automatically be installed as well by `raco`.

You will need to actually to install [CSFML][csfml] for your platform manually. This is just a simple matter of downloading the package for your platform and installing it appropriately. If you want to use audio with `r-cade`, you will also need to have [OpenAL][openal] and [libsndfile][libsndfile] as well.

*TODO: Maybe provide some detailed instructions on installing dynamic libraries?*

Be sure that all the dynamic libraries downloaded are available in your `PATH` (or whatever is correct for your platform).

*NOTE: I develop on Windows, but all the required dependencies are cross platform with MacOS X and Linux.*

## Quickstart

Once all the above has been done, you should be able to launch [Racket][racket] and try doing something very simple:

```racket
(require r-cade)

(define (my-game)
  (text 2 2 "Hello, world!")
  (wait)
  (quit))

(run my-game 128 128)
```

The above is about as simple as it gets. It should open a game window that has a resolution of 128 x 128 pixels and calls `my-game` at a rate of 60 times a second. However, the `wait` function will halt the game loop and wait until either the window is closed or one of the control pad buttons has been pressed (start, select, quit, z, or x). At which time, the game will quit.

---

For some more complicated code samples, check out the [examples folder in GitHub][examples] for some simple demos and even complete games like Breakout and Tetris showcasing the following features:

* Drawing 1-bit sprites, text, lines, rectangles, and circles
* Playing 8-bit music from notes
* Mouse, controller, and button presses
* Defining custom actions bound to buttons
* Voice instruments and envelopes
* Generating 8-bit waveforms and playing sounds

## Goals

As stated earlier, the three pillars `r-cade` is built on are:

* Designed for kids
* No external assets
* Retro

### Designed for Kids

I wanted to make something that could be as simple as QBASIC was for me when I was learning to program oh-so-many years ago. But, being inside [Racket][racket], as they become more comfortable, they can expand to a whole world of possibilities and learn some very powerful concepts.

### No External Assets

You will never find code in `r-cade` for loading of sprites, sounds, music, or any other kind of asset. This is for two reasons:

1. Often times what holds kids back is the idea that they can't draw and don't know how to make good sounds, etc.

2. Image and audio assets end up abstracting away much of what's actually happening under-the-hood. This is great later on, but if you begin too abstracted, I believe it can hinder long-term growth.

And because of this...

### Retro

For those of us who grew up programming the Trash-80, C64, Atari 2600, GameBoy, NES, etc. we learned a **lot** about what was really going on. I want kids today to be able to learn these things as well.

A great example of this is sound. How does your speaker _actually_ play a C# note? Programming is what taught me about wave frequencies, wave functions, sound envelopes, etc. Now, with `r-cade`, it's trivial to play around with these things and hear the differences immediately:

```racket
; create a sound wave that goes from 200 -> 100 Hz over 0.4 seconds
(define death-sound (sweep 200 100 0.4 #:instrument sawtooth-wave #:envelope z-envelope))
```

Even better, it's possible for them to create their own `instrument` and `envelope` functions and really start having fun learning about sound waves!

With drawing, I wanted every pixel to be a single bit and sprites to be as simple as the [CHIP-8][chip-8]. With 1-bit video memory it's pretty amazing what you can accomplish. The only difference here is I've added 16 colors into the mix. But, each sprite is a single color. Also, video memory is preserved just like on hardware. Unless it is cleared by the programmer, the bits aren't going anywhere.

[racket]: https://racket-lang.org/
[csfml]: https://www.sfml-dev.org/download/csfml/
[csfml-package]: https://pkgs.racket-lang.org/package/csfml
[openal]: https://www.openal.org/
[libsndfile]: http://www.mega-nerd.com/libsndfile/#Download
[examples]: https://github.com/massung/r-cade/tree/master/examples
[chip-8]: https://massung.github.io/CHIP-8/
[twinkle]: https://raw.github.com/massung/r-cade/master/screenshots/twinkle.gif
[breakout]: https://raw.github.com/massung/r-cade/master/screenshots/breakout.gif
[tetris]: https://raw.github.com/massung/r-cade/master/screenshots/tetris.gif
