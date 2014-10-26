{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Monoid
import qualified Data.Set             as S
import           Data.Text.Lazy       (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           System.IO
import           Test.Hspec

import Data.FCA

-- | A piece of test data.
v :: Context
v = V.fromList [ (1, S.fromList [2,3])
               , (2, S.fromList [1,2,3])
               , (3, S.fromList [1,2])
               , (4, S.fromList [3])
               , (5, S.fromList [2,3,5])
               ]

-- | Test data for translations.
names, names1, names2 :: Vector Text

names = V.fromList ["abcd", "defg", "abcd", "ghij", "abcd", "jklm"]
names1 = names
names2 = V.reverse names

-- | Test reading and parsing of input data.
formatsSpec :: Spec
formatsSpec = do
    describe "Value translation" $ do
        it "maintains lexicographic order of input values" $ do
            let (imap1, _omap1) = toTranslation names1
            let (imap2, _omap2) = toTranslation names2

            imap1 `shouldBe` imap2

        it "maintains lexicographic order of output values" $ do
            let (_imap1, omap1) = toTranslation names1
            let (_imap2, omap2) = toTranslation names2

            omap1 `shouldBe` omap2

    describe "Reading input formats" $ do
        it "should read EA and EAV identically" $ do
            -- Open all three input files.
            eaH  <- openFile "data/fruit.ea" ReadMode
            eavH <- openFile "data/fruit.eav" ReadMode

            -- Parse the CSV contents.
            eaCSV  <- either error id . decode NoHeader <$> BS.hGetContents eaH
            eavCSV <- either error id . decode NoHeader <$> BS.hGetContents eavH

            let ea  = parseEA eaCSV
            let eav = parseEAV eavCSV

            ea `shouldBe` eav

        it "should read EA and TAB identically" $ do
            -- Open all three input files.
            eaH  <- openFile "data/fruit.ea" ReadMode
            tabH <- openFile "data/fruit.csv" ReadMode

            -- Parse the CSV contents.
            eaCSV  <- either error id . decode NoHeader <$> BS.hGetContents eaH
            tabCSV <- either error id . decode NoHeader <$> BS.hGetContents tabH

            let ea  = parseEA eaCSV
            let tab = parseTabular tabCSV

            ea `shouldBe` tab

        it "should read EAV and TAB identically" $ do
            -- Open all three input files.
            eavH <- openFile "data/fruit.eav" ReadMode
            tabH <- openFile "data/fruit.csv" ReadMode

            -- Parse the CSV contents.
            eavCSV <- either error id . decode NoHeader <$> BS.hGetContents eavH
            tabCSV <- either error id . decode NoHeader <$> BS.hGetContents tabH

            let eav = parseEAV eavCSV
            let tab = parseTabular tabCSV

            eav `shouldBe` tab

        it "should read EAV data correctly" $ do
            pending
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

