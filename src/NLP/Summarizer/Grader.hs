{-# LANGUAGE OverloadedStrings #-}

module NLP.Summarizer.Grader where

import NLP.Summarizer.Article
import NLP.Summarizer.Dictionary (Dictionary, unimportantWords)
import NLP.Summarizer.Sentence
import NLP.Summarizer.Stemmer (stemStrip)
import NLP.Summarizer.Term

import Data.List ((\\))
import qualified Data.Text as T

grade :: Article -> Article
grade = applySentenceFactors
      . extractArticleConcepts
      . gradeSentences
      . createImportantWordsList

isNewParagraph :: Sentence -> Bool
isNewParagraph sentence =
  case terms sentence of
    w1:w2:_ ->
      T.isInfixOf "\n" (value w1) && T.isInfixOf "\n" (value w2)
    otherwise -> False

applySentenceFactors :: Article -> Article
applySentenceFactors article =
  let s:ss = sentences article
      s' = s { score = score s * 2 }
      factor :: Sentence -> Sentence
      factor sentence =
        if isNewParagraph sentence
        then sentence { score = score sentence * 1.6 }
        else sentence
      sentences' = map factor (s':ss)
  in article { sentences = sentences' }

extractArticleConcepts :: Article -> Article
extractArticleConcepts article =
  let importantWords' = importantWords article
      concepts' =
        if (length importantWords' > 5)
        then let baseFreq = termFrequency (importantWords' !! 5)
             in filter (\w -> termFrequency w >= baseFreq) importantWords'
        else importantWords'
  in article { concepts = map value concepts' }

gradeSentence :: Article -> Sentence -> Sentence
gradeSentence article sentence =
  let f word r =
        let wordstem = stemStrip (rules article) (value word)
            importantWord = any (\w -> (stem w) == wordstem)
                                (importantWords article)
        in if importantWord
           then r + 1
           else r
      score' = foldr f 0 (terms sentence)
  in sentence { score = score' }

gradeSentences :: Article -> Article
gradeSentences article =
  let sentences' = map (gradeSentence article) (sentences article)
  in article { sentences = sentences'}

createImportantWordsList :: Article -> Article
createImportantWordsList article =
  -- (\\) for lists is like set's difference operation
  article { importantWords =
              (wordCounts article) \\ (unimportantWords (rules article)) }

compareWordsByFrequency :: Word -> Word -> Int
compareWordsByFrequency lhs rhs = undefined
