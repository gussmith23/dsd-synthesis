# Synthesis for DNA Strand Displacement
=====================================
[![Build Status](https://travis-ci.org/morae/dsd-synthesis.svg?branch=master)](https://travis-ci.org/morae/dsd-synthesis)

## Introduction
Course project for CSE 507 at UW. This is a DNA Strand Displacement interpreter written in [Rosette](http://emina.github.io/rosette/) to allow for synthesis. See the [report](https://github.com/morae/dsd-synthesis/blob/master/report/report.tex) for more details.

## Building
With racket's command-line tools installed, run [`build.sh`](https://github.com/morae/dsd-synthesis/blob/master/build.sh) from the top level directory. This will build all the racket files and run all the tests.

## Demo
This project also contains a demo. The demo is located in [`src/demo.rkt`](https://github.com/morae/dsd-synthesis/blob/master/src/demo.rkt), and includes the ability to synthesize both AND gates and OR gates. In particular, the demo has:
- predicates that describe both an AND gate (`and-system?`) and an OR gate (`or-system?`)
- sketches of a human-designed AND gate (`sketch-1`) and a human-designed OR gate (`sketch-2`)
- an example human designed AND gate (`example-1`) and an example human-designed OR gate (`example-2`)
- a function that can be called on a predicate and a sketch to produce a gate that satisfies the predicate (`solve-system`)

To try out the demo, call `solve-system` on one of the predicates and one of the sketches, like this: `(solve-system and-system? sketch-1)`. That call will produce a gate that has the same semantics as the human-designed AND gate.

To test programs in the "real" DSD, go to [http://dsd.azurewebsites.net](
http://dsd.azurewebsites.net) (requires MS Silverlight).


## References
- Background information on DNA strand displacement
  - [Scaling Up Digital Circuit Computation with DNA Strand Displacement Cascades (Qian, Winfree)](http://science.sciencemag.org/content/332/6034/1196/tab-pdf)
- Information on the implemented DSD language
  - [Abstractions for DNA circuit design (Lakin, Youssef, Cardelli, Phillips)](http://rsif.royalsocietypublishing.org/content/early/2011/07/12/rsif.2011.0343)
  - [Abstractions for DNA circuit design Supplementary Material (Lakin, Youssef, Cardelli, Phillips)](http://rsif.royalsocietypublishing.org/highwire/filestream/22970/field_highwire_adjunct_files/0/rsif20110343supp1.pdf)
