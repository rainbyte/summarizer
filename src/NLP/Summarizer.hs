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
          -> Text -- ^ text to summarize
          -> IO SummarizerResult
summarize lang txt = do
  dict <- loadFromFile (T.unpack lang)
  let args = SummarizerArguments { dictionaryLanguage = lang
                                 , inputString = txt
                                 , ratio = RatioByPercent 10
                                 }
      art = (highlight args . grade . parseText txt . createArticle) dict
  pure SummarizerResult { summarySentences =
                            map originalSentence (sentences art)
                        , summaryKeywords = concepts art
                        }
