module Main where

import qualified Data.Set                   as S
import qualified Data.Vector                as V
import           System.IO
import           Test.Hspec

import           Data.FCA

-- | A piece of test data.
v :: Context
v = V.fromList [ (1, S.fromList [2,3])
               , (2, S.fromList [1,2,3])
               , (3, S.fromList [1,2])
               , (4, S.fromList [3])
               , (5, S.fromList [2,3,5])
               ]

-- | Test reading and parsing of input data.
formatsSpec :: Spec
formatsSpec =
    describe "reading input formats" $ do
        it "should read EAV data correctly" $ do
            input <- openFile "data/fruit.eav" ReadMode
            hClose input
            pendingWith "not implemented"

        it "should read tabular data correctly" $ do
            input <- openFile "data/fruit.csv" ReadMode
            hClose input
            pendingWith "not implemented"

        it "should read EA data correctly" $ do
            input <- openFile "data/fruit.ea" ReadMode
            hClose input
            pendingWith "not implemented"

main :: IO ()
main = do
    hspec formatsSpec

