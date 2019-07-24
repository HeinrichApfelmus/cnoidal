# cnoidal

*Cnoidal* is a Haskell library for representing, transforming, and live coding music.

The library is based on the notion of *temporal media*, a data type that represents a collection of time intervals tagged with values. It supports various functor instances, like Applicative Functor, which are highly useful for live coding.

A first version of the library is presented at the [FARM 2019][farm2019] workshop [at the ICFP 2019 in Berlin][berlin].

  [cnoidal]: https://en.wikipedia.org/wiki/Cnoidal_wave
  [farm2019]: https://icfp19.sigplan.org/details/farm-2019-papers/2/Demo-Functors-and-Music
  [berlin]: https://icfp19.sigplan.org/home

The name is inspired by the [cnoidal waveform][cnoidal], a nondispersive shape that water waves can take.

## Documentation

### Audio output

By itself, the `Cnoidal.Player` module only allows you to output MIDI messages. You will need a MIDI instrument to actually hear sound. However, you can find many software instruments on the internet. On the open source side, you can use

  * [fluidsynth][] — a synthesizer that uses Soundfonts.
  * [csound-expression][] — a Haskell library that wraps the [Csound][] language.

On the commercial side, you can use most digital audio workstations (DAW) or a performance-oriented application like [MainStage][].

  [fluidsynth]: http://www.fluidsynth.org
  [csound-expression]: https://hackage.haskell.org/package/csound-expression
  [csound]: https://csound.com
  [mainstage]: https://www.apple.com/mainstage/