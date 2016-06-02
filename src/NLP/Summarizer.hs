module NLP.Summarizer
  ( summarize
  , SummarizerResult(..))
  where

import NLP.Summarizer.Arguments
import NLP.Summarizer.Article
import NLP.Summarizer.Dictionary
import NLP.Summarizer.Grader
import NLP.Summarizer.Highlighter
import NLP.Summarizer.Sentence

import Data.Text (Text)
import qualified Data.Text as T

data SummarizerResult = SummarizerResult
  { summarySentences :: [Text]
  , summaryKeywords :: [Text]
  } deriving Show

summarize :: Text -- ^ dictionary to use (language)
          -> Int  -- ^ summarization % ratio
          -> Text -- ^ text to summarize
          -> SummarizerResult
summarize lang ratio' txt =
  let dict = loadFromFile (T.unpack lang)
      args = SummarizerArguments
        { dictionaryLanguage = lang
        , inputString = txt
        , ratio = RatioByPercent ratio'
        }
      art = (highlight args . grade . parseText txt . createArticle) dict
  in   SummarizerResult { summarySentences =
                            map originalSentence (sentences art)
                        , summaryKeywords = concepts art
                        }
