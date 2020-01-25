# Racket Cade (R-cade)

The `r-cade` package is a very simple game engine for the [Racket][racket] programming language with a few, specific goals in mind:

* Designed for kids; doing simple things is simple
* No external assets; all rendering, sound, and music is code
* Retro; it's intended to teach some low-level fundamentals

More on these later in the README. For now...

## Screenshots

Here are some of the example programs:

![The Classic... Tetris][tetris]
---
![Breakout!][breakout]

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

## Documentation

This section is a brief introduction of all the functions available.

### Main Game Loop

(**run** _loop width height #:scale 3 #:fps 60 #:title "R-cade"_)

Creates a new game window, video memory, and enters the main game loop. The _loop_ parameter is a function you provide, which will be called once per frame and should take no arguments. The _width_ and _height_ parameters define the size of video memory (not the size of the window!).

(**quit**)

Closes the window, which will terminate the main game loop.

(**wait** _[until btn-any]_)

Hard stops the game loop and waits until either the window is closed or the _until_ function returns true. While waiting, events are still processed.

(**sync**)

Called once per frame automatically by the main game loop. You shouldn't need to call this yourself unless you are creating your own game loop. It processes all events, renders video memory, and ensures the framerate is locked.

(**frame**)

Returns the current frame: 1, 2, 3, ....

(**width**)

Returns the width of video memory in pixels.

(**height**)

Returns the height of video memory in pixels.

(**flip**)

Renders video memory to the window. This is done automatically when `sync` is called (which is also automaticall called). You should never need to call this yourself.

### Inputs

All `btn-*` functions return `#f` if their respective button is not pressed, otherwise they return the count of how many frames the button has been pressed for. For exmaple `1` would indicate the button was _just_ pressed. This allows for easy testing of pressed, just pressed, or against a repeat rate.

(**btn-start**)

Returns the button state of the ENTER key.

(**btn-select**)

Returns the button state of the SPACEBAR key.

(**btn-quit**)

Returns the button state of the ESCAPE key.

(**btn-z**)

Returns the button state of the Z key.

(**btn-x**)

Returns the button state of the X key.

(**btn-up**)

Returns the button state of the UP arrow key.

(**btn-down**)

Returns the button state of the DOWN arrow key.

(**btn-right**)

Returns the button state of the RIGHT arrow key.

(**btn-left**)

Returns the button state of the LEFT arrow key.

(**btn-mouse**)

Returns the button state of the LEFT mouse button.

(**mouse-x**)

Returns the X coordinate (in pixels) of the current mouse location. 0 is the far left edge.

(**mouse-y**)

Returns the Y coordinate (in pixels) of the current mouse location. 0 is the top edge.

### Actions

Sometimes you want to be able to bind buttons to specific, named actions so your code is easier to read (and modify if you want to change your button mapping). To do this, use the `define-action` macro:

(**define-action** _name btn [rate]_)

Some examples:

```racket
; the (move-left) function will return #t if the left arrow key is pressed
(define-action move-left btn-left)

; the (jump) function will return #t if Z is key was just pressed
(define-action jump btn-z #t)

; the (fire) function will return #t while X is pressed at a rate of 5 times per second
(define-action fire btn-x 5)
```

### Drawing

(**cls**)

Clears video memory.

(**color** n)

Changes the active color to n (0-15). The default color palette is the same as the [PICO-8][pico-8]:

![](https://www.lexaloffle.com/gfx/pico8_pal_017.png)

(**set-color!** _n r g b_)

Change a color in the palette at index _n_ [0, 15] to the RGB byte values.

(**draw** _x y bytes_)

Uses the current color to render a 1-bit sprite composed of _bytes_ to video memory at (_x_,_y_). For example:

```racket
(draw 10 12 '(#b010 #b111 #b010))
```

The above would draw a 3x3 sprite that looks like + sign to the pixels at (10,12) -> (12,14). Any bit set will set the pixel in video memory to the current color. A clear bit is considered to be transparent and will not alter video memory.

(**text** _x y s_)

Draw the string _s_ (note: under the hood `(~a _s)` is used, so _s_ can be of any type) at (_x_,_y_). The default font is a fixed-width ASCII font with each character being 3x6 pixels in size.

(**line** _x1 y1 x2 y2_)

Draws a 1-pixel width line from (_x1_,_y1_) to (_x2_,_y2_) using the current color.

(**rect** _x y w h #:fill #f_)

Draw a rectangle starting at (_x_,_y_) with a size of (_w_,_h_). If _fill_ is `#t` then it will be a solid rectangle, otherwise just the outline.

(**circle** _x y r #:fill #f_)

Draw a circle starting at (_x_,_y_) with a radius of _r_ pixels If _fill_ is `#t` then it will be a solid circle, otherwise just the outline.

### Sounds

All audio is played by composing 8-bit PCM WAV data.

(**waveform** _curve seconds #:instrument sin #:envelope (const 1)_)

All sounds are made using the `waveform` function. The _curve_ argument is a function that is given a single value in the range of [0.0, 1.0] and should return a frequency to play at that time; 0.0 is the beginning of the waveform and 1.0 is the end. The _seconds_ parameter defines the length of the waveform.

The _instrument_ is the wave function to use and defaults to a simple sine wave. Other - built in - wave functions include `sawtooth-wave`, `square-wave`, `triangle-wave`, and `noise-wave`. But the user can define their own function and use it as well. The range of the parameter send to the _instrument_ function should be assumed [0.0, 2pi] and the return value should range from -1.0 to 1.0.

The _envelope_ is an amplitude multiplier. It is a function which - like _curve_ - is given a single value in the range [0.0, 1.0] indicating where in the sound it is. It should return a value in the range [0.0, 1.0], where 0.0 indicates a null amplitude and 1.0 indicates full amplitude. The default is to simply play the entire sound at full amplitude. Other, built-in envelopes include `z-envelope` and `s-envelope`. There is also an `envelope` function that helps with the creation of your own envelopes.

(**envelope** _y . ys_)

The `envelope` function returns a function that can be used as the _envelope_ paramater to any of the sound functions. It builds a simple, evenly spaced, linearly interpolated plot of amplitude envelopes. For example, the `z-envelope` is defined as:

```
(define z-envelope (envelope 1 1 0 0))
```

This means that in the time range of [0.0, 0.33] the sound will play at full amplitude. From [0.33, 0.66] the envelope will decay the amplitude linearly from 1.0 down to 0.0. Finally, from [0.66, 1.0] the amplitude of the sound will be forced to 0.0.

(**tone** _freq seconds #:instrument sin #:envelope (const 1)_)

This is a simple helper function for generating waveforms of a constant frequency.

(**sweep** _start end seconds #:instrument sin #:envelope (const 1)_)

A helper for creating a waveform that linearly interpolates over a duration of _seconds_ from _start_ Hz to _end_ Hz.

(**sound?** _sound_)

Returns `#t` if _sound_ is a valid waveform object.

(**play-sound** _waveform #:volume 100.0 #:pitch 1.0 #:loop #f_)

Uses one of 4 sound channels to play the waveform. If no channels are available then the sound will not be played. Returns the sound channel the waveform is playing on or `#f`.

(**stop-sound** _channel_)

Stops the waveform currently playing on the _channel_ returned with `play-sound`.

### Music

Music is created by parsing notes and creating an individual waveform for each note, then combining them together into a single waveform to be played on a dedicated music channel. Only one "tune" can be playing at a time.

(**make-tune** _notes #:bpm 160 #:instrument sin_)

Parses the _notes_ string and builds a waveform for each note. Notes are in the format `<key>[<octave>][<hold>]`. Examples:

* `"C#3--"` is a C-sharp in 3rd octave and held for a total of 3 quarter-notes time;
* `"Bb"` is a B-flat held for a single quarter-note and uses the octave of whatever note preceeded it;

The default octave is 4, but once an octave is specified for a note then that becomes the new default octave for subsequent notes.

How long each note is held for (in seconds) is determined by the _bpm_ (beats per minute) parameter. A single beat is assumed to be a single quarter-note. So, with a little math, the `C#--` at a rate of 160 BPM would play for 1.125 seconds (3 beats * 60 s/m ÷ 160 bpm). It is not possible to specify 1/8th and 1/16th notes. In order to achieve them, increase the _bpm_ appropriately.

All note waveforms are played with an ADSR (attack, decay, sustain, release) envelope. This cannot be overridden.

(**tune?** _tune_)

Returns `#t` if _tune_ is a valid music object.

(**play-music** _tune #:volume 100.0 #:pitch 1.0 #:loop #f_)

Stops any music currently playing and switches it to _tune_.

(**stop-music**)

Stops any music currently playing.

(**pause-music** _[pause #t]_)

If _pause_ is `#t` then the currently playing music is paused, otherwise it is resumed. If the music was not paused already and resumed, the music will restart from the beginning.

## Coming Features

Some things not yet implemented in the library, but planned:

* Gamepad support
* Custom fonts
* Channel mixing
* Fullscreen shader effects

Additionally, my next big feature will be a custom `#lang` for making `r-cade` games in a much more BASIC-like language.

## Goals

As stated earlier, the three pillars `r-cade` is built on are:

* Designed for kids
* No external assets
* Retro

### Designed for Kids

I wanted to make something that could be as simple as QBASIC was for me when I was learning to program oh-so-many years ago. But, being inside [Racket][racket], as they become more comfortable, they can expand to a whole world of possibilities and learn some very powerful concepts.

### No External Assets

You will never find external asset loading code in `r-cade` (e.g. sprites, sounds, levels, etc). This is for two reasons:

1. Often times what holds kids back is the idea that they can't draw or create good sounds, etc. Or they are lacking access to the tools to make them.

2. Image and audio assets end up abstracting away much of what's actually happening under-the-hood. This is great later on, but if you begin too abstracted, I believe it can hinder long-term growth.

And because of this...

### Retro

For those of us who grew up programming the Trash-80, C64, Atari 2600, GameBoy, NES, etc. we learned a **lot** about what was really going on: video memory, wave functions, interrupts, et al. I want kids today to be able to learn these things as well, just in a safer environment where they can experiment without having to worry about getting it "wrong".

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
[pico-8]: https://www.lexaloffle.com/pico-8.php
