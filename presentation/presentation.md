% A Complete Idiot's Guide to Formal Concept Analysis
% Thomas Sutton
% 2013-10-16

# Introduction

Volunteering to give at talk at [FP-Syd][] was supposed to motivate me
to get off my arse and finish a project I started a while ago.

Alas it didn't work and [language-css-attoparsec][] has seen too
little effort to demonstrate:

![](images/language-css-attoparsec.png)

[FP-Syd]: http://fp-syd.ouroborus.net
[language-css-attoparsec]: https://github.com/thsutton/language-css-attoparsec

I did, however, learn how to use [test-framework][] to run QuickCheck
and HUnit tests.

[test-framework]: http://hackage.haskell.org/package/test-framework

Instead I'll be talking about some Haskell code I wrote to perform
*formal concept analysis*.

# Caveats

- I'm pretty bad at programming in Haskell and the code I'm talking about
is pretty bad code.

- I'm pretty bad at mathematics and the explanation I give here is
probably pretty bad.

If you notice me say something wrong or silly please shout out and let
me know. Or talk to me afterwards.

# Formal Concept Analysis

Most of this comes from *Introduction to Lattices and Order* (second
edition) by B. A. Davey and H. A. Priestly. There's also
[Wikipedia article on formal concept analysis][wiki].

[wiki]: http://en.wikipedia.org/wiki/Formal_concept_analysis

*Formal concept analysis* is a mathematical formalism which analyses
the data in a *context* and attempts to extract the *concepts*
embodied within that data.

# Contexts

A *concept* is a structure which relates a set of objects with a set
of attributes.

$Context = (G, M, I)$

- The set of objects is called $G$ (from *gegenstände*)
- The set of attributes is called $M$ (from *merkmale*)
- The relationship is called $I$

# Concepts

A *concept* is a pair of sets: a set of objects and a set of
attributes (the *extent* and *intent* of the concept respectively).

$(A \subseteq G , B \subseteq M)$

Intuitively: 

- $A$ is the set of all objects which have all the attributes in $B$;
  and

- $B$ is the set of all attributes which apply to all objects in $A$.

(This has something to do with Galois connections and stuff. I don't
know what that means, but yeah.)

# Basic Algorithm

1. Construct an adjacency matrix of the object/attribute graph. Use a
attribute-major layout as this will be helpful later on.
   
2. Initialise a table of attributes/objects with a single "all the
things" value.

3. For each attribute in the matrix (in order of "maximal-ness"):

    1. If the extent of the attribute is already present in the table:

        1. Add the attribute we're processing to the existing table
        entry.

    2. Otherwise:

        1. Add an entry for the attribute to table.
	
	2. Find the intersection of the current attribute extent and
	each existing entry; add them to the table if they don't
	already exist.

4. Output the `dot` code for the graph:

    1. Each entry in the table is a node.

    2. Each node has edges to its parents (covering) according to the
    proper subset relation on the extent.

# The Code

Using the excellent [cassava][] package for CSV parsing.

[cassava]: http://hackage.haskell.org/package/cassava

# Example

Name               Colour   Type
-----------------  -------  -----
Pink Lady          Red      Apple
Granny Smith       Green    Apple
Golden Delicious   Yellow   Apple
Red Delicious      Red      Apple
Lemon              Yellow   Citrus
Orange             Orange   Citrus
Mandarin           Orange   Citrus
Lime               Green    Citrus

# The Fruit Lattice

![](../example/fruit.png)
