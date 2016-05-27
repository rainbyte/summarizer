{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}

module NLP.Summarizer.Dictionary
  ( Dictionary (..)
  , loadFromFile
  )
  where

import NLP.Summarizer.Term

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.HXT.Core

data Dictionary =
  Dictionary { unimportantWords :: [Term]
             , lineBreakRules :: [Text]
             , notALineBreakRules :: [Text]
             , depreciateValue :: [Text]
             , termFreqMultiplierRule :: [Text]
             , step1PrefixRules :: Map Text Text
             , step1SuffixRules :: Map Text Text
             , manualReplacementRules :: Map Text Text
             , prefixRules :: Map Text Text
             , suffixRules :: Map Text Text
             , synonymRules :: Map Text Text
             , language :: Text
             }
  deriving Show

-- Create a dictionary based on language dependent XML
loadFromFile :: String -> IO Dictionary
loadFromFile lang = do
  let filename = "dicts/" ++ lang ++ ".xml"
  [dict] <- runX (readDocument [withValidate no] filename >>> processXml)
  return dict

toText :: (ArrowXml a) => a XmlTree Text
toText = getChildren >>> getText >>> arr T.pack

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

across :: (ArrowXml a) => Text -> a XmlTree [Text]
across = listA
       . (>>> toText)
       . foldr1 (/>)
       . map (atTag . T.unpack)
       . T.splitOn "/"

toMap :: [Text] -> Map Text Text
toMap =
  let mapSnd f (a, b) = (a, f b)
      split = mapSnd (T.drop 1) . T.breakOn "|"
  in Map.fromList . fmap split

toTerms :: [Text] -> [Term]
toTerms = fmap (\w -> Term { value = w, stem = "", termFrequency = 0 })

processXml :: ArrowXml a => a XmlTree Dictionary
processXml =
  proc doc -> do
    unimportantWords' <- across "grader-tc/word" -< doc
    lineBreakRules' <- across "parser/linebreak/rule" -< doc
    notALineBreakRules' <- across "parser/linedontbreak/rule" -< doc
    depreciateValue' <- across "grader-syn/depreciate/rule" -< doc
    termFreqMultiplierRule' <- across "grader-tf/word" -< doc
    step1PrefixRules' <- across "stemmer/step1_pre/rule" -< doc
    step1SuffixRules' <- across "stemmer/step1_post/rule" -< doc
    manualReplacementRules' <- across "stemmer/manual/rule" -< doc
    prefixRules' <- across "stemmer/pre/rule" -< doc
    suffixRules' <- across "stemmer/post/rule" -< doc
    synonymRules' <- across "stemmer/synonyms/rule" -< doc
    language' <- (getChildren >>> getAttrValue "lang" >>> arr T.pack) -< doc
    returnA -< Dictionary
                 { unimportantWords = toTerms unimportantWords'
                 , lineBreakRules = lineBreakRules'
                 , notALineBreakRules = notALineBreakRules'
                 , depreciateValue = depreciateValue'
                 , termFreqMultiplierRule = termFreqMultiplierRule'
                 , step1PrefixRules = toMap step1PrefixRules'
                 , step1SuffixRules = toMap step1SuffixRules'
                 , manualReplacementRules = toMap manualReplacementRules'
                 , prefixRules = toMap prefixRules'
                 , suffixRules = toMap suffixRules'
                 , synonymRules = toMap synonymRules'
                 , language = language'
                 }
