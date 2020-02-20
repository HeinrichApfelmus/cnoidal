# cnoidal

*Cnoidal* is a Haskell library for representing, transforming, and live coding music.

The library is based on the notion of *temporal media*, a data type that represents a collection of time intervals tagged with values. It supports various functor instances, like Applicative Functor, which are highly useful for live coding.

The name is inspired by the [cnoidal waveform][cnoidal], a nondispersive shape that water waves can take.

  [cnoidal]: https://en.wikipedia.org/wiki/Cnoidal_wave

## Documentation

### Talk at FARM 2019

A first version of the library was presented at the [FARM 2019][farm2019] workshop [at the ICFP 2019 in Berlin][berlin]. This presentation focuses on the *temporal media* data type, which represents music as a collection of time intervals tagged with values. It supports an Applicative Functor instance which is highly useful for live coding. However, a lawful Monad instance can only be defined for some variants of the data type. After introducing these functor instance, I gave a hands-on demonstration and live coded some music.

* Paper: [Demo: Functors and Music [pdf]](https://apfelmus.nfshost.com/temp/functors-n-music-farm2019.pdf)
* Video: [Heinrich Apfelmus: Functors and Music [YouTube]](https://www.youtube.com/watch?v=2zh8KfdlgJ8)
* Slides: [slides-functors-and-music.pdf [pdf]](doc/farm2019/slides-functors-and-music.pdf)
* Performance: One of a kind, never to be repeated. But here is a very similar worksheet that I created in preparation for the performance: [not-the-performance.hhs [HyperHaskell worksheet]](doc/farm2019/not-the-performance.hhs)

  [farm2019]: https://icfp19.sigplan.org/details/farm-2019-papers/2/Demo-Functors-and-Music
  [berlin]: https://icfp19.sigplan.org/home

### Audio output

By itself, the `Cnoidal.Player` module only allows you to output MIDI messages. You will need a MIDI instrument to actually hear sound. However, you can find many software instruments on the internet. On the open source side, you can use

  * [fluidsynth][] — a synthesizer that uses Soundfonts.
  * [csound-expression][] — a Haskell library that wraps the [Csound][] language.

On the commercial side, you can use most digital audio workstations (DAW) or a performance-oriented application like [MainStage][].

  [fluidsynth]: http://www.fluidsynth.org
  [csound-expression]: https://hackage.haskell.org/package/csound-expression
  [csound]: https://csound.com
  [mainstage]: https://www.apple.com/mainstage/