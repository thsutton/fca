module Main ( main ) where

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Csv
import           Data.Map
import           Data.Set
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word

import           FCA (main)

-- | 32 bits ought to be enough for anyone.
type Attributes = Word32

-- We'll re-use 'Name' from Data.Csv.
-- type Name = ByteString

-- | A context represents the complete information from which we can derive
-- concepts.
data Context = Context { ctxObj :: Set Name
                       , ctxAtt :: Map Attributes (Name, Name)
                       , ctxRel :: Map Name Attributes
                       }

-- | A concept is composed of a set of objects and a set of properties s.t. all
-- of the properties hold of all of the objects.
data Concept = Concept { extent :: Set Name
                       , intent :: Attributes
                       }

main2 :: IO ()
main2 = do
  input <- B.getContents
  case decode False input of
    Left err -> putStrLn err
    Right csv -> do
      let hd = V.head csv
          csv' = V.tail csv
      print $ V.length hd
      print $ V.length $ massage csv'

-- | Massage the input data so that it has attributes.
massage :: Vector (Vector ByteString) -> Vector (ByteString)
massage = V.map (V.head)

