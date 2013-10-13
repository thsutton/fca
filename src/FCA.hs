{-# LANGUAGE OverloadedStrings #-}
{- |
Module: FCA
Maintainer: Thomas Sutton

This module implements an algorithm to perform Formal Concept Analysis on a
concept. The algorithm is that described in /Introduction to Lattices and Order/
(second edition) by B. A. Davey and H. A. Priestly.

This implementation is rather hackish and probably quite inefficient but
helped me (rather: is helping me) to understand to topic.

-}
module FCA where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- * Data types

-- | We'll just use integers to identify attributes.
type AttrId = Int

-- | Similarly for objects.
type ObjId = Int

-- | An attribute is an ID together with a set of objects which have the
-- attribute.
type Attribute = (AttrId, Set ObjId)

-- | A Context is an incidence matrix between objects and attributes. It's
-- attribute major.
type Context = Vector (Attribute)

-- | An AETable is the internal datastructure of the algorithm.
type AETable = Vector AERow

-- | An AERow represents a point in the concept lattice with the given
-- attributes and the extent.
--
-- These values are not, themselves, necessarily concepts; they merely
-- represent them in the lattice.
type AERow = (Set AttrId, Set ObjId)

-- * Public

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

-- * Internal

-- | Parse CSV data into a context.
--
-- This function converts a vector of object-records into a context and a pair
-- of maps which can be used to recover the human-readable names for output.
--
-- Note that this function assumes that the input data is rectangular.
parseContext :: Vector (Vector ByteString) -> (Context, Map ObjId Name, Map AttrId Name)
parseContext csv = let hd      = V.tail $ V.head csv
                       bd      = V.map V.tail $ V.tail csv
                       no      = V.length bd
                       na      = V.length hd
                       ctx     = V.generate na (\a -> (a, V.ifoldl (\s i v -> if (B.null $ v V.! a) then s else S.insert i s) S.empty bd))
                       objmap  = V.ifoldl (\m i v -> M.insert i (v V.! 1) m) M.empty bd
                       attrmap = V.ifoldl (\m i n -> M.insert i n m) M.empty hd
                   in (ctx, objmap, attrmap)


-- | Construct the attribute/extent table of a context.
buildAETable :: Context -> AETable
buildAETable ctx = snd $ work ctx $ V.singleton (S.empty, S.empty)
  where
    work :: Context -> AETable -> (Context, AETable)
    work ctx table = maybe (ctx, table)
                     (\(a, ctx') -> work ctx' $ insertAttr a table)
                     (chooseMax ctx)

-- | Generate 'dot' code of the concept lattice based on a attribue/extent
-- table.
--
-- Points in the generated graph are labelled with names from the input data.
generateGraph :: AETable          -- ^ Context lattice table.
              -> Map ObjId Name   -- ^ Map from object ID to name.
              -> Map AttrId Name  -- ^ Map from attribute ID to name.
              -> Text
generateGraph table omap amap = let cs = map graphNode $ zip [0..] $ V.toList table
                                    b = fromText "digraph {\n\trankdir=BT;\n" `mappend`
                                        mconcat cs `mappend`
                                        fromText "}\n"
                                in toLazyText b
  where graphNode (i, (as, ns)) = let label = fromText "label=\"" `mappend` decimal (i) `mappend` fromText "\""
                                  in mconcat [ fromText "\tconcept" , decimal i
                                             , fromText " [ "
                                             , label
                                             , fromText " ];\n"
                                             ]
        
-- | Select the maximal attribute in the context.
--
-- Here, maximal means is not a proper subset of any other element.
chooseMax :: Context -> Maybe (Attribute, Context)
chooseMax ctx = fmap (flip vselect ctx) $ V.findIndex (f ctx) ctx
  where
    f ctx a = maybe True (const False) $
              V.findIndex (\b-> (snd a) `S.isProperSubsetOf` (snd b)) ctx

-- | Insert a new attribute and it's extent into the table.
--
-- If the extent is already present, the existing row will be labelled with the
-- additional attribute; otherwise, a new row will be added.
insertAttr :: Attribute -> AETable -> AETable
insertAttr (attr, ext) table = 
  let t = table
      i = V.findIndex (\r -> ext == snd r) table
  in case i of
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
                              new = V.filter (flip V.notElem current) $ V.map (S.intersection e) current
                          in (t V.++) $ V.map (\x -> (S.empty, x)) new

-- * Utilities

-- | Extract and remove the element with the specified index from a vector.
vselect :: Int -- ^ The index.
        -> Vector a -- ^ The vector.
        -> (a, Vector a) -- ^ The value and new vector.
vselect i v = (v V.! i , V.ifilter (\j _ -> i /= j) v)

-- * Tests

-- | A piece of test data.
v :: Context
v = V.fromList [ (1, S.fromList [2,3])
               , (2, S.fromList [1,2,3]) 
               , (3, S.fromList [1,2])
               , (4, S.fromList [3])
               , (5, S.fromList [2,3,5])
               ]
