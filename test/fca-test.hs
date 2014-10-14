module Main where

import qualified Data.Set                   as S
import qualified Data.Vector                as V

import Data.FCA

-- | A piece of test data.
v :: Context
v = V.fromList [ (1, S.fromList [2,3])
               , (2, S.fromList [1,2,3])
               , (3, S.fromList [1,2])
               , (4, S.fromList [3])
               , (5, S.fromList [2,3,5])
               ]

main :: IO ()
main = do
  putStrLn "Test!"
  print v

