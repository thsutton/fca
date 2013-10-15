% A Complete Idiot's Guide to Formal Concept Analysis (in Haskell)
% Thomas Sutton
% 2013-10-16

# Introduction

Volunteering to talk at [FP-Syd][] was supposed to motivate me to finish a
project I started a while ago. Alas it didn't work and, while I learned how to
use [test-framework][], [language-css-attoparsec][] has seen too little effort
to be work demonstrating:

![](images/language-css-attoparsec.png)

[FP-Syd]: http://fp-syd.ouroborus.net
[language-css-attoparsec]: https://github.com/thsutton/language-css-attoparsec
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

*Formal concept analysis* is a mathematical formalism which analyses the data in
a *context* and attempts to extract the *concepts* embodied within that data.

Relating it to similar techniques, *formal concept analysis* might be thought of
as the love child of *decision tree learning* and *k-means clustering*.

# Contexts

A *context* is a structure which relates a set of objects with a set of
attributes.

Formally, a context is a triple:

$$(G, M, I)$$

- $G$ (from *gegenstände*) is the set of objects;

- $M$ (from *merkmale*) is the set of attributes; and

- $I$ is the relation linking $G$ and $M$.

# Context Table

Name               C=R C=G C=Y C=O T=A T=C
-----------------  --- --- --- --- --- ---
Pink Lady           X               X
Granny Smith            X           X
Golden Delicious            X       X
Red Delicious       X               X
Lemon                       X           X
Orange                          X       X
Mandarin                        X       X
Lime                    X               X

# Concepts

A *concept* (with respect to some context) is a pair of sets: a set of objects
and a set of attributes (the *extent* and *intent* of the concept respectively).

$$(A \subseteq G , B \subseteq M)$$

- $A$ is the set of all objects which have all the attributes in $B$;
  and

- $B$ is the set of all attributes which apply to all objects in $A$.

(This has something to do with Galois connections and stuff. I don't
know what that means, but yeah.)

# Basic Algorithm

1. Construct an adjacency matrix of the context.

2. Initialise a table of attributes/objects. Add the initial "all the things"
   entry.

3. For each attribute, in order of "maximal-ness":

	1. If the extent is already present, add the attribute to that row.
	2. Otherwise:
	    1. Add a new row.
		2. Add the intersections with previous rows (if not already present).

4. Add a final "none of the things" entry.

4. Generate `dot` code of the lattice:

    1. Each row becomes a node (labelled w/ attributes); and
	2. Each node has edges to it's *covering* nodes (immediate, proper
	   supersets).

# The Code

- Parse CSV input using the [cassava][] package.
- Use `containers` and `vector` for data representation.
- (Investigate using `Data.Bits` instead of `Data.Set` in future.)

[cassava]: http://hackage.haskell.org/package/cassava

# Example: Fruit Data

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

# Example: Fruit Context

Obj   cy  cg  cr  co  ta  tc
---   --  --  --  --  --  --
GD    ☑                ☑    
GS        ☑            ☑    
RD            ☑        ☑    
PL            ☑        ☑    
L     ☑                    ☑
O                 ☑        ☑
M                 ☑        ☑
Li        ☑                ☑

# Example: Fruit Attribute/Extent Table


     Attributes  Objects
---  ----------  ---------------------
  1              GD,GS,RD,PL,Le,O,M,Li
  2  cy          GD,Le
  3  cg          GS,Li
  4  ta          GD,GS,RD,PL
  5              GD
  6              GS
  7  cr          RD,PL
  8  tc          Le,O,M,Li
  9              Le
 10              Li
 11  co          O,M
 12              ∅


# Example: Fruit Concept Lattice

![](../example/fruit.png)

# Code: Types

````{.haskell}
type Name = Text

type AttrId = Int
type ObjId = Int

-- | An attribute is an ID together with a set of objects which have the
-- attribute.
type Attribute = (AttrId, Set ObjId)

-- | A Context is an incidence matrix between objects and attributes. It's
-- attribute major.
type Context = Vector (Attribute)

-- | An AERow represents a point in the concept lattice with the given
-- attributes and the extent.
type AERow = (Set AttrId, Set ObjId)

-- | An AETable is the internal datastructure of the algorithm.
type AETable = Vector AERow
````

# Code: Main

````{.haskell}
parseContext :: Vector (Vector Name) -> (Context, Map ObjId Name, Map AttrId Name)
buildAETable :: Context -> AETable
generateGraph :: AETable          -- ^ Context lattice table.
              -> Map ObjId Name   -- ^ Map from object ID to name.
              -> Map AttrId Name  -- ^ Map from attribute ID to name.
              -> Text

main :: IO ()
main = do
  input <- BL.getContents
  case decode False input of
    Left err -> error err
    Right csv -> let (ctx, omap, amap) = parseContext csv
                     table = buildAETable ctx
                     graph = generateGraph table omap amap
                 in do
                   T.putStrLn graph
````

# Code: Building the table

````{.haskell}
-- | Construct the attribute/extent table of a context.
buildAETable :: Context -> AETable
buildAETable ctx = let g = V.foldl (\s v-> S.union s $ snd v) S.empty ctx
                       t = snd $ work ctx $ V.singleton (S.empty, g)
                   in t V.++ V.singleton (S.empty, S.empty)
  where
    work :: Context -> AETable -> (Context, AETable)
    work ctx table = maybe (ctx, table)
                     (\(a, ctx') -> work ctx' $ insertAttr a table)
                     (chooseMax ctx)

-- | Insert a new attribute and it's extent into the table.
insertAttr :: Attribute -> AETable -> AETable
insertAttr (attr, ext) table =
  case V.findIndex (\r -> ext == snd r) table of
    -- Add the label to the existing row.
    Just j -> labelAttr j attr table
    -- Add a new row to the table.
    Nothing -> addIntersects attr ext $ extendTable attr ext table
  where
    extendTable :: AttrId -> Set ObjId -> AETable -> AETable
    extendTable a e t = V.snoc t (S.singleton a, e)
    labelAttr j a t = let (attr, extent) = t V.! j
                          r = (S.insert a attr, extent)
                      in table V.// [(j, r)]
    addIntersects :: AttrId -> Set ObjId -> AETable -> AETable
    addIntersects a e t = let current = V.map (snd) t
                              new = V.filter (\s -> (not $ S.null s) && (V.notElem s current)) $ V.map (S.intersection e) current
                          in (t V.++) $ V.map (\x -> (S.empty, x)) new
````

# Code: LOLWUT

````{.haskell}
-- | Find the smallest set containing an element.
--
-- This is partial.
smallestWith :: Ord e => e -> Set (Set e) -> Set e
smallestWith e ss = let ss' = S.filter (S.member e) ss
                    in head $ sortBy myCmp $ S.toList ss'
  where myCmp x y = compare (S.size x) (S.size y)

-- | Filter the elements @e@ of @s@ by whether or not @m@ maps @e@ to @s@.
filterSetByMap :: Ord i => Set i -> Map i (Set i) -> Set i
filterSetByMap s m = S.filter (\e -> (m M.! e) == s) s 

-- | Find nodes which cover another.
covering :: Set Int                     -- ^ Extent to cover.
         -> [(Int, (Set Int, Set Int))] -- ^ Candidates.
         -> [(Int, (Set Int, Set Int))]
covering s ss = let ss' = filter (myCmp (1,(S.empty,s))) ss
                in filter (\c -> null $ filter ((flip myCmp c)) ss') ss'
  where myCmp = \(_,(_,s)) (_,(_,t)) -> s `S.isProperSubsetOf` t
````

# Sources

![](images/cover.jpg)

Most of this comes from *Introduction to Lattices and Order* (second
edition) by B. A. Davey and H. A. Priestly. There's also
[Wikipedia article on formal concept analysis][wiki].

[wiki]: http://en.wikipedia.org/wiki/Formal_concept_analysis
