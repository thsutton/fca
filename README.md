Formal Concept Analysis
=======================

This repository contains a small implementation of an algorithm to perform
*formal concept analysis* as well as a presentation about it to be given at the
FP-Syd functional programming group in Sydney on 16 Oct 2013.

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

The presentation can be found in the `presentation/` directory along with a
`Makefile` which will convert the Markdown text of `presentation.md` into HTML.

The code is commented and the presentation gives some of the details of the
algorithm and its implementation.
