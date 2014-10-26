Formal Concept Analysis
=======================

[![Build Status][badge]][build-log]

[build-log]: https://travis-ci.org/thsutton/fca
[badge]: https://travis-ci.org/thsutton/fca.png?branch=master

This repository contains a small implementation of an algorithm to
analyse a dataset and construct a concept lattice using an algorithm
described in [Introduction to Lattices and Order][book] by Davey and
Priestley (Chapter 3: Formal Concept Analysis).

[book]: http://amzn.to/1nZgMdu

Installation
------------

The `fca` software is written in Haskell and can be compiled using the normal
Haskell tools:

````{.shell}
cabal configure
cabal build
cabal test
cabal haddock --executables
cabal install
````

Usage
-----

The package includes a library which implements and exports almost all
functionality and an executable, called `fca`, which provides a command-line
interface.

````
fca - formal concept analysis

Usage: fca [-f|--format ea|eav|tab] [-o|--output FILE] [FILE]
  Generate the concept lattice which describs a data set.

Available options:
  -h,--help                Show this help text
  -f,--format ea|eav|tab   Input data format. (default: EAV)
  -o,--output FILE         Write output to FILE. (default: stdout)
  FILE                     Read input from FILE. (default: stdin)
````

Input data must be supplied in one of three similar CSV formats:

- entity-attribute format consists of two columns: an object name and an
attribute name.

- entity-attribute-value format consists of three columns: an object name, an
attribute name, and an attribute value. The attribute name and value will be
concatenated to form an attribute suitable for processing.

- tabular format consists of a matrix with objects as rows and attributes as
columns; the header row and column contain the names, and a non-empty cell
represents the presence of an attribute at an object.

Two examples data sets, each in all three formats, can be found in the `data/`
directory:

- The `.txt` file contains a summary of the data;

- The `.tab` file contains the tabular format;

- The `.ea` file contains the entity-attribute format; and

- The `.eav` file contains the entity-attribute-value format.

In all three cases, the object and attribute names have been abbreviated and,
when processed, result in the same output.

The output is a graph of the concept lattice in [Graphviz][graphviz] format.

[graphviz]: http://graphviz.org/
