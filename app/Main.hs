{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import NLP.Summarizer

import Control.Monad
import qualified Data.Text.IO as TIO
import Options.Generic

data CliOptions = CliOptions
  { ratio :: Maybe Int <?> "summarization % [default = 20%]"
  , dictionary :: Maybe String <?> "dictionary to use"
  , input :: Text <?> "text to summarize"
  , output :: Maybe FilePath <?> "output file [default = stdout]"
  , html :: Bool <?> "output as html"
  , keywords :: Bool <?> "only output keywords"
  , about :: Bool <?> "only output the summary"
  , version :: Bool <?> "show version information"
  } deriving (Generic, Show)

instance ParseRecord CliOptions

main :: IO ()
main = do
  x <- (getRecord "Test program" :: IO CliOptions)

  let txt = unHelpful (input x)

  -- let txt = "Ojalá esa comisión cumpla con su deber. Hoy en día no prosperan los negocios pequeños en Argentina gracias a los hermosos oligopolios que se han armado las grandes empresas, que les encanta aprovecharse de su posición.\nSí, las estoy mirando a ustedes telefónicas, si no fuera por ustedes tendría 300mb de internet hasta en Jujuy."
  TIO.putStrLn txt

  result <- summarize "es" txt
  forM_ (summarySentences result) TIO.putStrLn
  forM_ (summaryKeywords result) TIO.putStrLn

  pure ()
