module NLP.Summarizer.Sentence where

import NLP.Summarizer.Term

import Data.Text (Text)

data Sentence =
  Sentence { terms :: [Term]
           , score :: Double
           , selected :: Bool
           , wordCount :: Int
           , originalSentence :: Text
           }
  deriving Show
