Formal Concept Analysis
=======================

[![Build Status](https://travis-ci.org/thsutton/fca.png?branch=master)](https://travis-ci.org/thsutton/fca)

This repository contains a small implementation of an algorithm to
analyse a dataset and construct a concept lattice using an algorithm
described in [Introduction to Lattices and Order][book] by Davey and
Priestley (Chapter 3: Formal Concept Analysis).

[book]: http://amzn.to/1nZgMdu

Software
--------

The `fca` software is written in Haskell and can be compiled using the normal
Haskell tools:

````{.shell}
cabal configure
cabal build
cabal test
cabal haddock --executables
cabal install
````

Presentation
------------

I gave a presentation about this software to the FP-Syd functional
programming group in Sydney in November 2013. Some of the materials
can be found in the `presentation/` directory along with a `Makefile`
which will convert the Markdown text of `presentation.md` into HTML.

The code is commented and the presentation gives some of the details
of the algorithm and its implementation.
