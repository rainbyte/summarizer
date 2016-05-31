{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import NLP.Summarizer

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import Options.Generic

data CliOptions = CliOptions
  { ratio :: Maybe Int <?> "summarization % [default = 20%]"
  , dictionary :: Maybe Text <?> "dictionary to use"
  , input :: Text <?> "text to summarize"
--  , output :: Maybe FilePath <?> "output file [default = stdout]"
--  , html :: Bool <?> "output as html"
  , keywords :: Maybe Bool <?> "only output keywords"
--  , about :: Bool <?> "only output the summary"
--  , version :: Bool <?> "show version information"
  } deriving (Generic, Show)

instance ParseRecord CliOptions

main :: IO ()
main = do
  cliOptions <- (getRecord "Test program" :: IO CliOptions)

  let txt = unHelpful (input cliOptions)
      dict = fromMaybe "es" $ unHelpful (dictionary cliOptions)
      ratio' = fromMaybe 20 $ unHelpful (ratio cliOptions)
      showKeywords = fromMaybe False $ unHelpful (keywords cliOptions)

  result <- summarize dict ratio' txt
  mapM_ TIO.putStrLn (summarySentences result)
  when showKeywords $
    mapM_ TIO.putStrLn (summaryKeywords result)

  pure ()
