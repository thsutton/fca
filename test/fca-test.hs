module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Monoid
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
        it "should read test data identically in all three formats" $ do
            -- Open all three input files.
            eaH  <- openFile "data/fruit.ea" ReadMode
            eavH <- openFile "data/fruit.eav" ReadMode
            tabH <- openFile "data/fruit.csv" ReadMode

            -- Parse the CSV contents.
            eaCSV  <- either error id . decode NoHeader <$> BS.hGetContents eaH
            eavCSV <- either error id . decode NoHeader <$> BS.hGetContents eavH
            tabCSV <- either error id . decode NoHeader <$> BS.hGetContents tabH

            let ea  = parseEA eaCSV
            let eav = parseEAV eavCSV
            let tab = parseTabular tabCSV

            ea `shouldBe` eav
            eav `shouldBe` tab

        it "should read EAV data correctly" $ do
            input <- openFile "data/fruit.eav" ReadMode
            csv <- decode NoHeader <$> BS.hGetContents input
            let frame = either (error) (parseEAV) csv
            frame `shouldBe` (Frame mempty mempty mempty)

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

