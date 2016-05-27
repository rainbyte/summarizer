module NLP.Summarizer.Stemmer
  ( stemWord
  , stemStrip )
  where

import NLP.Summarizer.Dictionary
import NLP.Summarizer.Term

import Data.Map
import qualified Data.Text as T
import Data.Text (Text)

stemWord :: Dictionary -> Text -> Term
stemWord rules word =
  let word' = T.toLower word
  in Term { value = stemFormat rules word'
          , stem = stemStrip rules word'
          , termFrequency = 1 }

stemStrip :: Dictionary -> Text -> Text
stemStrip rules word =
  let strip = (replaceWord $ synonymRules rules)
            . (stripSuffix $ suffixRules rules)
            . (stripPrefix $ prefixRules rules)
            . (replaceWord $ manualReplacementRules rules)
            . (stemFormat rules)
      word' = strip word
  in if T.length word' <= 2
     then stemFormat rules word
     else word'

stemFormat :: Dictionary -> Text -> Text
stemFormat rules = (stripSuffix $ step1SuffixRules rules)
                 . (stripPrefix $ step1PrefixRules rules)

stripSuffix :: Map Text Text -> Text -> Text
stripSuffix suffixRule word =
  let f k v r = case T.stripSuffix k r of
                  Just r' -> T.append r' v
                  Nothing -> r
  in foldrWithKey f word suffixRule

replaceWord :: Map Text Text -> Text -> Text
replaceWord replacementRule word =
  foldrWithKey (\k v r -> if word == k then v else r) word replacementRule

stripPrefix :: Map Text Text -> Text -> Text
stripPrefix prefixRule word =
  let f k v r = case T.stripPrefix k r of
                  Just r' -> T.append v r'
                  Nothing -> r
  in foldrWithKey f word prefixRule
