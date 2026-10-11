{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | "Did you mean" suggestions.
--
-- When a KRDICT lookup finds no entry at all, offer the closest words from the
-- Korean list bundled with the app (data/topik-vocab) so a user can recover a
-- wrong two-beolsik spelling without re-typing anything.
--
-- This is deliberately self-contained: a pure Levenshtein distance, a small
-- ranking helper, and a loader that follows the same file/decode convention
-- as the repository layer. No external fuzzy-matching dependency.
module Output.Dict.Suggest
    ( levenshtein
    , suggestions
    , loadLocalWords
    ) where

import Control.Monad (forM)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Output.Domain.Types (TOPIK_Level(..), VocabularyCard(..))

-- | Classic Levenshtein edit distance: the minimum number of single-character
-- insertions, deletions, or substitutions needed to change @xs@ into @ys@.
levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein xs ys = d (length xs) (length ys)
  where
    cost i j | xs !! (i - 1) == ys !! (j - 1) = 0
             | otherwise                        = 1
    d i j
      | i == 0    = j
      | j == 0    = i
      | otherwise = min (d (i - 1) j + 1)
                    (min (d i (j - 1) + 1)
                         (d (i - 1) (j - 1) + cost i j))

-- | Up to five words from @list@ that are one edit away from @query@ (a
-- substitution, insertion, or deletion), ordered closest first (ties keep
-- their original list order). Returns @[]@ when nothing is that close, so an
-- unrelated query yields no spurious suggestions.
--
-- One edit catches a single wrong jamo (e.g. typing 우이 for 오이) without
-- surfacing words that only vaguely resemble the query.
suggestions :: [Text] -> Text -> [Text]
suggestions list query
    | T.null query   = []
    | null list      = []
    | best > thr     = []
    | otherwise      = take 5 [ w | (d, _, w) <- ranked, d <= thr ]
  where
    qs        = T.unpack query
    thr       = if length qs <= 2 then 1 else 2
    cands     = [ (levenshtein qs (T.unpack w), i, w) | (i, w) <- zip [(0 :: Int) ..] list ]
    ranked    = sortOn (\(d, i, _) -> (d, i)) cands
    best      = case ranked of
        []           -> maxBound
        (d, _, _):_  -> d

-- | Every word in the app's TOPIK vocabulary corpus
-- (data/topik-vocab/level{1..6}.json), flattened into a plain word list. This
-- is the candidate pool that 'suggestions' ranks against.
loadLocalWords :: IO [Text]
loadLocalWords = do
    let dir = "data/topik-vocab"
    perLevel <- forM [One .. Six] $ \level -> do
        let filePath = dir </> ("level" <> show (fromEnum level + 1) <> ".json")
        exists <- doesFileExist filePath
        if not exists
            then pure []
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just (cards :: [VocabularyCard]) -> pure (map korean cards)
                    Nothing -> pure []
    pure (concat perLevel)
