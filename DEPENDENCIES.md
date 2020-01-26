# Installing R-cade Dependencies

R-cade uses [CSFML][csfml] under-the-hood. While using `raco` into install the `r-cade` package will automatically install the `csfml` _bindings_ for Racket, it will not install the required dynamic libraries on your system.

The instructions for installing the dynamic libraries will be different for each platform. This document hopes to make installing them quite painless.

*NOTE: I develop on Windows, so if any of the instructions for MacOS X or Linux are incorrect or could be easier, please send me an email or a PR with the changes to this document!*

## List of Required Dynamic Libraries

* [CSFML][csfml]
* [OpenAL][openal]
* [libsndfile][libsndfile]

*NOTE: On Windows, after installing all the dynamic libraries and updating your `PATH` environment variable so it knows where to find them, you will need to close any terminal applications or [Racket][racket] and relaunch them so they get the updated environment.*

## Installing CSFML

The current [Racket][racket] bindings are for version 2.5 of [CSFML][csfml].

### Windows

Download either the 32-bit or 64-bit [CSFML][csfml] library, depending on whether you have the 32-bit or 64-bit [Racket][racket] installed.

*NOTE: If you do not know whether you have 32- or 64-bit [Racket][racket] installed, simply launch [Racket][racket] and execute `(system-info 'word)` to find out.*

Unzip the downloaded library anywhere you want (I personally unzipped it to C:\CSFML). Then edit your environment variables and add `<path-to-CSFML>\bin` to your `PATH`. This will ensure that the dynamic libraries can be found.

### MacOS X

To install the [CSFML][csfml] dynamic libraries on OS X, just use the [Homebrew][brew] formula:

```bash
$ brew install csfml
```

### Linux

TODO:

## Installing OpenAL

[OpenAL][openal] is required for audio to play. If you do not care about audio in your games, you may skip installing it.

### Windows

As with CSFML, simply download the [Windows installer](http://openal.org/downloads/) and run it. This will install the [OpenAL][openal] dynamic library to your `Windows\System32` folder and you won't need to do anything else.

### MacOS X

[OpenAL][openal] can be installed on OS X using [Homebrew][brew]:

```bash
$ brew install csfml
```

### Linux

Assuming an Ubuntu distribution, you should be able to install the [OpenAL][openal] dynamic libraries with `apt-get`:

```bash
$ sudo apt-get install sudo libopenal-dev
```

## libsndfile

As with [OpenAL][openal], [libsndfile][libsndfile] is required for audio to play. If you do not care about audio in your games, you may skip installing it.

### Windows

Head over to [libsndfile][libsndfile] and download either the 32- or 64-bit installer and run it. By default, this will install [libsndfile][libsndfile] (64-bit) to `C:\Program Files\Mega-Nerd\libsndfile` (the 32-bit version will install to `Program Files (x86)`.

As with [CSFML][csfml], you'll need to add the `<libsndfile>\bin` to your `PATH` environment variable so the dynamic library can be found by `r-cade`.

### MacOS X

[libsndfile][libsndfile] can be installed on OS X using [Homebrew][brew]:

```bash
$ brew install libsndfile
```

### Linux

Assuming Ubuntu, you should be able to install [libsndfile][libsndfile] using `apt-get`:

```bash
$ sudo apt-get install libsndfile1
```

[racket]: https://racket-lang.org/
[csfml]: https://www.sfml-dev.org/download/csfml/
[csfml-package]: https://pkgs.racket-lang.org/package/csfml
[openal]: https://www.openal.org/
[libsndfile]: http://www.mega-nerd.com/libsndfile/#Download
[twinkle]: https://raw.github.com/massung/r-cade/master/screenshots/twinkle.gif
[breakout]: https://raw.github.com/massung/r-cade/master/screenshots/breakout.gif
[tetris]: https://raw.github.com/massung/r-cade/master/screenshots/tetris.gif
[pico-8]: https://www.lexaloffle.com/pico-8.php
[dependencies]: https://github.com/massung/r-cade/tree/master/DEPENDENCIES.md
[brew]: https://brew.sh/
