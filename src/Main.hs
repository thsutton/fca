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
module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.FCA
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as T

main :: IO ()
main = do
  input <- BL.getContents
  case decode NoHeader input of
    Left err -> error err
    Right csv -> let (ctx, omap, amap) = parseContext csv
                     table = buildAETable ctx
                     graph = generateGraph table omap amap
                 in do
                   T.putStrLn graph
