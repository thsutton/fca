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
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Csv hiding (Parser)
import           Data.FCA
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as T
import           Options.Applicative
import           System.IO

-- | Data formats
data Format
    = EA      -- ^ Data is in entity/attribute pairs.
    | EAV     -- ^ Data is in entity, attribute, value triples.
    | Tabular -- ^ Data is in column-per-attribute.
  deriving (Show, Ord, Eq)

data Options = Options
    { optVerbose :: Bool -- ^ Be verbose.
    , optHeader  :: Bool -- ^ Data includes headers.
    , optFormat  :: Format -- ^ Input data format.
    , optOutput  :: Maybe FilePath -- ^ Output to file.
    , optInput   :: Maybe FilePath -- ^ Input from file.
    }
  deriving (Eq, Ord, Show)

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
    outputP = option (return . Just) $
           long "output"
        <> short 'o'
        <> help "Output file."
        <> metavar "FILE"
        <> value Nothing
    inputP = argument (return . Just) $
           metavar "FILE"
        <> value Nothing
    formatP = option readFmt $
           long "format"
        <> short 'f'
        <> help "Input data format."
        <> metavar "av|eav|tab"
        <> value EAV
        <> showDefault
    readFmt :: (Monad m, MonadPlus m) => String -> m Format
    readFmt "ea" = return EA
    readFmt "eav" = return EAV
    readFmt "tab" = return Tabular
    readFmt _ = mzero

-- | Open input and output handles based on command-line options.
getHandles :: Options -> IO (Handle, Handle)
getHandles Options{..} = do
    inH <- maybe (return stdin) (flip openFile ReadMode) optInput
    outH <- maybe (return stdout) (flip openFile WriteMode) optOutput
    return (inH, outH)

main :: IO ()
main = do
    opt <- execParser opts
    print opt
    (inH, outH) <- getHandles opt
    input <- BL.hGetContents inH
    case decode NoHeader input of
        Left err -> error err
        Right csv -> let (ctx, omap, amap) = parseContext csv
                         table = buildAETable ctx
                         graph = generateGraph table omap amap
                     in do
                        T.hPutStrLn outH graph
    hClose inH
    hClose outH
  where
    opts = info (helper <*> optionsP)
        ( fullDesc
        <> progDesc "Perform formal concept analysis a data set."
        <> header "fca - formal concept analysis"
        )
