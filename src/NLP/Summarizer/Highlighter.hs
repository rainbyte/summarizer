
module NLP.Summarizer.Highlighter where

import NLP.Summarizer.Article
import NLP.Summarizer.Arguments
import NLP.Summarizer.Sentence (score, selected, terms, wordCount)

import Data.List (foldl', sortOn)
import Data.Ord (Down(..))

highlight :: SummarizerArguments -> Article -> Article
highlight args =
  case ratio args of
    RatioByLines n | n > 0 -> selectNumberOfSentences n
    RatioByPercent n | n > 0 -> selectSentencesByPercent n
    _ -> id

selectSentencesByPercent :: Int -> Article -> Article
selectSentencesByPercent percent article =
  let sentencesByScore = sortOn (Down . score) (sentences article)
      totalWords = foldr (\s r -> length(terms s) + r) 0 (sentences article)
      maxWords = truncate ( fromIntegral totalWords
                          * fromIntegral percent / 100)
      f (r, n) s = if n >= maxWords
                   then (r, n)
                   else (r ++ [s { selected = True }], n + wordCount s)
      (sentences', _) = case sentencesByScore of
        (x:xs) ->
          foldl' f ([x { selected = True }], wordCount x) xs
        [] -> ([], 0)
  in article { sentences = sentences' }

selectNumberOfSentences :: Int -> Article -> Article
selectNumberOfSentences lineCount article =
  let sentencesByScore = sortOn (Down .score) (sentences article)
      (xs,ys) = splitAt lineCount sentencesByScore
      xs' = map (\s -> s { selected = True }) xs -- TODO: filter empty from xs
  in article { sentences = xs' ++ ys }
