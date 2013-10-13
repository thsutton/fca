Formal Concept Analysis
=======================

This is a short talk about formal concept analysis in which I deploy to
aphorisms:

- Those who can do, those who can't teach.

- The best way to learn is by teaching.

**Please note**: This README is not an accurate description of the code.

Basic Algorithm
---------------

A `Context` is, essentially, a "set" of objects, a "set" of attributes, and a
relation between them. 

We'll build this in Haskell as a `Text` name to represent objects, values of
some type with an instance of `Bits` to represent sets of attributes, and a
`Map` from one to the other to represent the whole context:

````{.haskell}
data Context = Map Text Int
````

We'll accept our input in a some what friendlier format of CSV where each row
represents an object and each column represents a property. We'll convert each
unique value for each property into an attribute. For example, this following
data forms a context with four attributes:

Name               Colour   Type
-----------------  -------  -----
Pink Lady          Pink     Apple
Granny Smith       Green    Apple
Golden Delicious   Yellow   Apple

The attributes are:

1. Colour=Pink
2. Colour=Green
3. Colour=Yellow
4. Type=Apple

In converting these to out `Bits` values, we'll just assign them a bit as we
encounter them. This results in our data looking something like this:

Name                 Attributes
-----------------  ------------ 
Pink Lady                `1001`
Granny Smith             `0101`
Golden Delicious         `0011`

With each bit mapping to one of the four attributes described above.

Forming Candidates
------------------

A *concept* is a pair of sets: a set of objects (the *extent*) and a set of
attributes (the *intent*). Each of these sets is determined by the other:

- The extent is the set of all objects which have all of the attributes in the
  intent.

- The intent is the set of all attributes which hold of all the objects in the
  extent.

Given these two sets determine each other, we'll focus on one side and deploy
some very dumb, very brute force.

Let's generate all the possible candidates by enumerating the powerset of the
set of attributes. Then we'll check to see if this forms an interesting concept
(i.e. that there isn't a bigger set of attributes with the same extent)

It's pretty important to realise that this algorithm is excedingly shit and
we're using it because I'm too bad a programmer and no sort of mathematician at
all to be able to figure out a more efficient approach.
