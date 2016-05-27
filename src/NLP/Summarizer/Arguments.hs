module NLP.Summarizer.Arguments where

import Data.Text (Text)

data Ratio = RatioByPercent Int | RatioByLines Int
  deriving Show

data SummarizerArguments =
  SummarizerArguments { dictionaryLanguage :: Text
                      , inputString :: Text
                      , ratio :: Ratio
                      }
  deriving Show
