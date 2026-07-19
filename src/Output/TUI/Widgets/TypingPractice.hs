{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Widgets.TypingPractice
    ( -- * Main Drawing Functions
      drawTypingPractice
    , drawLevelSelector
    , drawTypingStats
      -- * Helper Widgets
    , drawCurrentPrompt
    , drawRealTimeInput
    , drawComposedInput
    , drawProgressBar
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intersperse)

import Output.Domain.Jamo
import Output.Domain.TypingLevel
import Output.Domain.TypingWord
import Output.Domain.TypingExercise
import Output.Domain.Types (TypingProgress(..))
import Output.TUI.Types
import Output.TUI.Widgets.Keyboard

-- | Draw the main typing practice screen
drawTypingPractice :: TypingState -> Widget Name
drawTypingPractice ts =
    vBox
        [ drawHeader ts
        , padAll 1 $ drawCurrentPrompt ts
        , padLeftRight 2 $ drawRealTimeInput ts
        , padTop (Pad 1) $ drawKeyboard (makeKeyboardState ts)
        , padTop (Pad 1) $ hCenter $ drawTypingStats (typStats ts)
        ]

-- | Draw the header with level and progress
drawHeader :: TypingState -> Widget Name
drawHeader ts =
    hBox
        [ withAttr titleAttr $ txt $ "Level " <> T.pack (show $ tlNumber $ typLevel ts) <> ": " <> tlName (typLevel ts)
        , fill ' '
        , drawProgressBar completed total
        ]
  where
    completed = typCurrentIndex ts
    total = length (typPrompts ts)

-- | Draw the current prompt
drawCurrentPrompt :: TypingState -> Widget Name
drawCurrentPrompt ts =
    case getCurrentPrompt ts of
        Nothing -> hCenter $ txt "Session complete!"
        Just prompt ->
            borderWithLabel (txt $ " " <> exerciseTypeLabel (typExerciseType ts) <> " ") $
                padAll 1 $ vBox
                    [ hCenter $ drawQuestion prompt
                    , hCenter $ padTop (Pad 1) $ drawTarget prompt
                    , if typShowHints ts
                        then hCenter $ padTop (Pad 1) $ drawHint prompt
                        else emptyWidget
                    ]

-- | Get current prompt
getCurrentPrompt :: TypingState -> Maybe TypingPrompt
getCurrentPrompt ts
    | typCurrentIndex ts < length (typPrompts ts) =
        Just (typPrompts ts !! typCurrentIndex ts)
    | otherwise = Nothing

-- | Draw the question/prompt
drawQuestion :: TypingPrompt -> Widget n
drawQuestion prompt = case tpExerciseType prompt of
    Echo -> withAttr promptAttr $ txt $ composeJamoSequence (twJamo (tpWord prompt))
    Build -> withAttr promptAttr $ txt $ "English: " <> twEnglish (tpWord prompt)
    Drill -> withAttr promptAttr $ txt $ composeJamoSequence (twJamo (tpWord prompt))
    Sentence -> withAttr promptAttr $ txt $ twEnglish (tpWord prompt)

-- | Draw target Korean text
drawTarget :: TypingPrompt -> Widget n
drawTarget prompt
    | tpShowKorean prompt = txt $ composeJamoSequence (twJamo (tpWord prompt))
    | otherwise = emptyWidget

-- | Draw hint (romanization)
drawHint :: TypingPrompt -> Widget n
drawHint prompt = case tpHint prompt of
    Just hint -> withAttr hintAttr $ txt $ "(" <> hint <> ")"
    Nothing -> emptyWidget

-- | Draw real-time input with composed Korean text
drawRealTimeInput :: TypingState -> Widget Name
drawRealTimeInput ts =
    hCenter $ hLimit 60 $ vLimit 5 $ borderWithLabel (txt " Your Input ") $ padAll 1 $
        vBox
            [ hCenter $ drawComposedInput ts
            , hCenter $ padTop (Pad 1) $ drawJamoInput ts
            , fill ' '
            ]

-- | Draw composed Korean text (main display)
drawComposedInput :: TypingState -> Widget Name
drawComposedInput ts
    | null typed = withAttr hintAttr $ txt "_"
    | otherwise = hBox [composedWidget, cursorWidget]
  where
    typed = typTypedJamo ts
    expected = case getCurrentPrompt ts of
        Just prompt -> tpExpectedJamo prompt
        Nothing -> []
    statuses = validateTyping expected typed

    -- Compose the typed jamo into Korean text
    composedText = composeJamoSequence typed

    -- Determine overall status for composed text
    -- Green if all correct so far, red if any incorrect
    hasIncorrect = any (== Incorrect) statuses
    composedAttr = if hasIncorrect then incorrectAttr else correctAttr

    composedWidget = withAttr composedAttr $ txt composedText

    -- Cursor
    cursorWidget
        | length typed < length expected = withAttr hintAttr $ txt "_"
        | otherwise = emptyWidget

-- | Draw raw jamo sequence (smaller, below composed text)
drawJamoInput :: TypingState -> Widget Name
drawJamoInput ts
    | null typed = emptyWidget
    | otherwise = withAttr hintAttr $ txt $ "(" <> jamoText <> ")"
  where
    typed = typTypedJamo ts
    expected = case getCurrentPrompt ts of
        Just prompt -> tpExpectedJamo prompt
        Nothing -> []
    statuses = validateTyping expected typed

    -- Show raw jamo with status indicators
    jamoText = T.pack $ zipWith statusChar typed statuses

    statusChar jamo Correct = jamoToChar jamo
    statusChar jamo Incorrect = jamoToChar jamo
    statusChar jamo Pending = jamoToChar jamo

-- | Convert Jamo to character for display
jamoToChar :: Jamo -> Char
jamoToChar (Consonant c) = c
jamoToChar (Vowel c) = c
jamoToChar (DoubleConsonant c) = c

-- | Draw a simple progress bar
drawProgressBar :: Int -> Int -> Widget n
drawProgressBar current total
    | total <= 0 = txt "[--------] 0%"
    | otherwise = txt $ "[" <> bar <> "] " <> T.pack (show pct) <> "%"
  where
    pct = if total > 0 then (current * 100) `div` total else 0
    filled = (current * 8) `div` max 1 total
    bar = T.replicate filled "■" <> T.replicate (8 - filled) "□"

-- | Draw typing statistics
drawTypingStats :: TypingStats -> Widget n
drawTypingStats stats =
    hBox $ intersperse (txt "  |  ")
        [ txt $ "WPM: " <> T.pack (show (round (tsWPM stats) :: Int))
        , txt $ "Accuracy: " <> T.pack (show (round (tsAccuracy stats) :: Int)) <> "%"
        , txt $ "Streak: " <> T.pack (show (tsStreak stats))
        , txt $ "Words: " <> T.pack (show (tsCorrectWords stats)) <> "/" <> T.pack (show (tsWordsCompleted stats))
        ]

-- | Create keyboard state from typing state
makeKeyboardState :: TypingState -> KeyboardState
makeKeyboardState ts = KeyboardState
    { ksAvailableKeys = tlKeys (typLevel ts)
    , ksNextKey = nextExpectedKey
    , ksLastKeyState = lastKeyState
    , ksShowQwerty = False
    }
  where
    typed = typTypedJamo ts
    expected = case getCurrentPrompt ts of
        Just prompt -> tpExpectedJamo prompt
        Nothing -> []

    nextExpectedKey
        | length typed < length expected = Just (expected !! length typed)
        | otherwise = Nothing

    lastKeyState = case (typLastKeyCorrect ts, lastTypedKey) of
        (Just True, Just k) -> Just (k, KeyCorrect)
        (Just False, Just k) -> Just (k, KeyError)
        _ -> Nothing

    lastTypedKey = if null typed then Nothing else Just (last typed)

-- | Exercise type label
exerciseTypeLabel :: TypingExerciseType -> Text
exerciseTypeLabel Echo = "Echo"
exerciseTypeLabel Build = "Build"
exerciseTypeLabel Drill = "Drill"
exerciseTypeLabel Sentence = "Sentence"

-- | Draw level selector screen (hierarchical view)
drawLevelSelector :: TypingState -> TypingProgress -> Widget Name
drawLevelSelector ts progress = case typLevelSelectMode ts of
    TopLevelSelect -> drawMainLevels (typSelectedIndex ts) progress
    SubLevelSelect parentLevel -> drawSubLevels parentLevel (typSelectedIndex ts) progress

-- | Draw main levels (top-level view)
drawMainLevels :: Int -> TypingProgress -> Widget Name
drawMainLevels selectedIdx progress =
    borderWithLabel (txt " Select Level ") $ padAll 1 $
        vBox $ zipWith (drawMainLevelOption selectedIdx progress) [0..] mainLevels

-- | Draw a main level option with completion count
drawMainLevelOption :: Int -> TypingProgress -> Int -> TypingLevel -> Widget Name
drawMainLevelOption selectedIdx progress idx level =
    let isSelected = idx == selectedIdx
        indicator = if isSelected then "▶ " else "  "
        levelNum = tlNumber level
        -- Show [completed/total] for levels with sub-levels
        subProgress = case getSubLevels levelNum of
            [] -> ""
            subs ->
                let completed = length $ filter (isLevelCompleted progress . tlNumber) subs
                    total = length subs
                in " [" <> T.pack (show completed) <> "/" <> T.pack (show total) <> "]"
        attr = if isSelected then menuSelectedAttr else menuAttr
    in withAttr attr $ txt $ indicator <> tlName level <> subProgress

-- | Draw sub-levels for a parent level
drawSubLevels :: Int -> Int -> TypingProgress -> Widget Name
drawSubLevels parentLevel selectedIdx progress =
    let subs = getSubLevels parentLevel
        parentName = case getLevel parentLevel of
            Just l -> tlName l
            Nothing -> "Level " <> T.pack (show parentLevel)
    in borderWithLabel (txt $ " " <> parentName <> " ") $ padAll 1 $
        vBox $ zipWith (drawSubLevelOption selectedIdx progress) [0..] subs

-- | Draw a sub-level option with completion star
drawSubLevelOption :: Int -> TypingProgress -> Int -> TypingLevel -> Widget Name
drawSubLevelOption selectedIdx progress idx level =
    let isSelected = idx == selectedIdx
        indicator = if isSelected then "▶ " else "  "
        -- ★ for completed, ☆ for not completed
        completionStar = if isLevelCompleted progress (tlNumber level)
                         then "★ "
                         else "☆ "
        attr = if isSelected then menuSelectedAttr else menuAttr
    in withAttr attr $ txt $ indicator <> completionStar <> tlName level

-- | Check if a level has been completed
isLevelCompleted :: TypingProgress -> Int -> Bool
isLevelCompleted progress levelNum = Set.member levelNum (tpCompletedLevels progress)
