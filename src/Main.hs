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
import qualified Data.Text.Lazy.IO    as T
import           Options.Applicative
import           System.IO

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

-- | Read data in entity-attribute format.
readEA :: Options -> Handle -> IO Frame
readEA _ inH = do
    input <- BL.hGetContents inH
    case decode NoHeader input of
        Left err -> error err
        Right csv -> return $ parseEA csv

-- | Read data in tabular format.
readTabular :: Options -> Handle -> IO Frame
readTabular _ inH = do
    input <- BL.hGetContents inH
    case decode NoHeader input of
        Left err -> error err
        Right csv -> return $ parseTabular csv

main :: IO ()
main = do
    opt <- execParser opts
    (inH, outH) <- getHandles opt

    -- Read the input data.
    Frame ctx omap amap <- getReader opt inH
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
