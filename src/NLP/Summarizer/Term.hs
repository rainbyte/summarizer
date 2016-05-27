module NLP.Summarizer.Term where

import Data.Text (Text)

data Term =
  Term { value :: Text
       , stem :: Text
       , termFrequency :: Double
       }
  deriving Show

instance Eq Term where
  t1 == t2 = (value t1) == (value t2)
