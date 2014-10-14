{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module: Main
Maintainer: Thomas Sutton

This module implements a command-line tool to perform formal concept analysis
on data sets.

-}
module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Csv hiding (Parser, Name)
import           Data.List
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO    as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Options.Applicative
import           System.IO

import Debug.Trace

import           Data.FCA

-- | Options for invocation, generally constructed from command-line options.
data Options = Options
    { optVerbose :: Bool -- ^ Be verbose.
    , optHeader  :: Bool -- ^ Data includes headers.
    , optFormat  :: Format -- ^ Input data format.
    , optOutput  :: Maybe String -- ^ Output to file.
    , optInput   :: Maybe String -- ^ Input from file.
    }
  deriving (Eq, Ord, Show)

-- | Formats for input data.
data Format
    = EA      -- ^ Data is in entity/attribute pairs.
    | EAV     -- ^ Data is in entity, attribute, value triples.
    | Tabular -- ^ Data is in column-per-attribute.
  deriving (Show, Ord, Eq)

-- | Parser 'Options' from command-line arguments.
optionsP :: Parser Options
optionsP = Options <$> verboseP
                   <*> headerP
                   <*> formatP
                   <*> outputP
                   <*> inputP
  where
    verboseP = switch $ 
           long "verbose"
        <> short 'v'
        <> help "Produce verbose output."
    headerP = switch $
           long "header"
        <> short 'H'
        <> help "Input contains headers."
    outputP = option (Just <$> str) $
           long "output"
        <> short 'o'
        <> help "Output file."
        <> metavar "FILE"
        <> value Nothing
    inputP = argument (Just <$> str) $
           metavar "FILE"
        <> value Nothing
    formatP = option readFmtP $
           long "format"
        <> short 'f'
        <> help "Input data format."
        <> metavar "av|eav|tab"
        <> value EAV
        <> showDefault
    readFmtP :: ReadM Format
    readFmtP = eitherReader readFmt
    readFmt "ea"  = return EA
    readFmt "eav" = return EAV
    readFmt "tab" = return Tabular
    readFmt "tabular" = return Tabular
    readFmt _     = Left "Format must be: ea, eav, tabular"

-- | Open input and output handles based on command-line options.
getHandles :: Options -> IO (Handle, Handle)
getHandles Options{..} = do
    inH <- maybe (return stdin) (flip openFile ReadMode) optInput
    outH <- maybe (return stdout) (flip openFile WriteMode) optOutput
    return (inH, outH)

-- * Reading data

type ObjectID = Int
type AttributeID = Int

-- | A 'Frame' couples 
data Frame = Frame
    { frameContext :: Context
    , frameObjects :: Map ObjectID Name
    , frameAttributes :: Map AttributeID Name
    }
  deriving (Show, Eq)

-- | Get a function to read data in the specified format.
getReader :: Options -> (Handle -> IO Frame)
getReader opt = case optFormat opt of
    EAV -> readEAV opt
    EA -> readEA opt
    Tabular -> readTabular opt

-- | Read data in entity-attribute-value format.
readEAV :: Options -> Handle -> IO Frame
readEAV _ inH = do
    input <- BL.hGetContents inH
    case decode NoHeader input of
        Left err -> error err
        Right csv -> return $ parseEAV csv
  where
    toMap :: Vector Text -> (Map Int Text, Map Text Int)
    toMap v =
        let l = zip [0..] $ nub $ V.toList v
            it = foldl (\m (i,n)-> M.insert i n m) M.empty l
            ti = foldl (\m (i,n)-> M.insert n i m) M.empty l
        in (it, ti)
    parseEAV :: (Vector (Name, Name, Name)) -> Frame
    parseEAV csv =
        let csv' = V.map (\(n,a,v) -> (n, T.concat [a,"=",v])) csv
            (omap, romap) = toMap $ V.map fst csv'
            (amap, ramap) = toMap $ V.map snd csv'
            fn (o,a) m = M.alter (Just . maybe (S.singleton o) (S.insert o)) a m
            ctx' = V.foldr fn M.empty csv' 
            ctx = V.fromList $ sort $ map (\(k,v) -> (fromJust (M.lookup k ramap), S.map (fromJust . flip M.lookup romap) v )) $ M.toList ctx'
        in Frame ctx omap amap


-- | Read data in entity-attribute format.
readEA :: Options -> Handle -> IO Frame
readEA _ _ = error "EA format is not supported."

-- | Read data in tabular format.
readTabular :: Options -> Handle -> IO Frame
readTabular _ inH = do
    input <- BL.hGetContents inH
    case decode NoHeader input of
        Left err -> error err
        Right csv ->
            let (ctx, omap, amap) = parseContext csv
            in return $ Frame ctx omap amap

main :: IO ()
main = do
    opt <- execParser opts
    (inH, outH) <- getHandles opt

    -- Read the input data.
    frame@(Frame ctx omap amap) <- getReader opt inH
    hClose inH

    -- Run the FCA algorithm on the context.
    let table = buildAETable ctx
    let graph = generateGraph table omap amap

    -- Output the graph.
    T.hPutStrLn outH graph
    hClose outH
  where
    opts = info (helper <*> optionsP)
        ( fullDesc
        <> progDesc "Perform formal concept analysis a data set."
        <> header "fca - formal concept analysis"
        )
