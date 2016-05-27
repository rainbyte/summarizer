{-# LANGUAGE OverloadedStrings #-}

module NLP.Summarizer.Article
  ( Article (..)
  , createArticle
  , parseText )
  where

import NLP.Summarizer.Dictionary ( Dictionary
                                 , lineBreakRules
                                 , notALineBreakRules
                                 )
import NLP.Summarizer.Sentence
import NLP.Summarizer.Stemmer (stemWord)
import NLP.Summarizer.Term

import qualified Data.List.Split as S
import Data.Text (Text)
import qualified Data.Text as T

data Article =
  Article { sentences :: [Sentence]
          , lineCount :: Int
          , concepts :: [Text]
          , rules :: Dictionary
          , importantWords :: [Term]
          , wordCounts :: [Term] }
  deriving Show

createArticle :: Dictionary -> Article
createArticle rules =
  Article { sentences = []
          , lineCount = 0
          , concepts = []
          , rules = rules
          , importantWords = []
          , wordCounts = [] }

parseText :: Text -> Article -> Article
parseText text art =
  let words' = T.split (\x -> elem x [' ', '\r']) text
      words = map (\w -> case T.stripPrefix "\n" w of
                           Just w' -> if T.length w > 2 then w' else w
                           Nothing -> w)
                  words'
      splitKeepDelim = S.split . S.keepDelimsR . S.whenElt
      wordsPerSentence = splitKeepDelim (isSentenceBreak (rules art)) words
      toSentence ws =
        let originalSentence' = T.intercalate " " ws
            ws' = map (\w -> Term { value = w, stem = "", termFrequency = 0})
                      ws
        in Sentence { terms = ws'
                    , originalSentence = originalSentence'
                    , score = 0
                    , selected = False
                    , wordCount = length ws'
                    }
      sentences' = map toSentence wordsPerSentence
      art' = foldr addWordCount art words
  in art' { sentences = sentences' }

addWordCount :: Text -> Article -> Article
addWordCount word art =
  if elem word ["", " ", "\n", "\t"]
  then art
  else let stemmedWord = stemWord (rules art) word
           (xs, ys) = break (\w -> stem w == stem stemmedWord) (wordCounts art)
           wordCounts' =
             case ys of
               [] -> stemmedWord:(wordCounts art)
               (y:ys') -> let y' = y { termFrequency = (termFrequency y) + 1 }
                          in xs ++ (y' : ys')
       in art { wordCounts = wordCounts' }

-- True: word contains "\r" or "\n"
--     | word ends with any lineBreakRules
--     | word starts with any notALinebreakrules
-- False: otherwise
isSentenceBreak :: Dictionary -> Text -> Bool
isSentenceBreak rules word =
   ( T.isInfixOf (T.pack "\r") word
  || T.isInfixOf (T.pack "\n") word
  || any (\p -> T.isSuffixOf p word) (lineBreakRules rules)
  || any (\p -> T.isPrefixOf p word) (notALineBreakRules rules))
