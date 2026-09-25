{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Draw
    ( drawUI
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Data.Text as T
import Data.Text (Text)

import Output.TUI.Types
import Output.Domain.Exercise (ExercisePrompt(..))
import Output.Domain.Types (TypingProgress, VocabularyState(..), MasteryLevel(..))
import Output.Domain.Sentence
    ( Sentence(..), SentenceVersion(..), QuestState(..), QuestPhase(..)
    , getLatestVersion, getBuiltChunks, getCurrentInput, getTypedJamo
    )
import Output.Domain.Jamo (allConsonants, allVowels, allDoubleConsonants)
import Output.TUI.Widgets.TypingPractice
import Output.TUI.Widgets.Keyboard (drawKeyboard, KeyboardState(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Main draw function
drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = case asScreen s of
        MainMenuScreen -> drawMainMenu s
        DrillScreen -> case asDrill s of
            Just drill -> drawDrill drill
            Nothing -> drawMainMenu s
        TypingPracticeScreen -> case asTyping s of
            Just ts -> drawTypingPracticeScreen ts
            Nothing -> drawMainMenu s
        TypingLevelSelectScreen -> case asTyping s of
            Just ts -> drawLevelSelectScreen ts (asTypingProgress s)
            Nothing -> drawMainMenu s
        ProgressScreen -> drawProgress s
        HelpScreen -> drawHelp
        QuitConfirmScreen -> drawQuitConfirm
        SentenceQuestScreen -> case asQuestState s of
            Just qs -> drawSentenceQuest s qs
            Nothing -> drawMainMenu s
        SentenceListScreen -> drawSentenceList s

-- | Draw main menu
drawMainMenu :: AppState -> Widget Name
drawMainMenu s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Output - Korean Learning ") $
    vBox
        [ padAll 2 $ center $ vBox
            [ withAttr titleAttr $ txt "Welcome to Output!"
            , padTop (Pad 1) $ txt "Learn Korean through active practice"
            ]
        , hBorder
        , padAll 1 $ drawMenuItems s
        , hBorder
        , padAll 1 $ drawStats s
        ]

-- | Draw menu items
drawMenuItems :: AppState -> Widget Name
drawMenuItems s = vBox $ zipWith (drawMenuItem (asMenuIndex s)) [0..] menuOptions
  where
    menuOptions =
        [ ("Sentence Quest", "Build Korean sentences word by word")
        , ("Typing Practice", "Learn Korean keyboard with guided levels")
        , ("Typing Drill", "Practice typing Korean words")
        , ("Reading Drill", "Read Korean and self-grade")
        , ("Writing Drill", "Translate English to Korean")
        , ("View Progress", "See your learning statistics")
        , ("Help", "View keyboard shortcuts")
        , ("Quit", "Exit the application")
        ]

drawMenuItem :: Int -> Int -> (Text, Text) -> Widget Name
drawMenuItem selected idx (name, desc)
    | selected == idx = withAttr menuSelectedAttr $
        hBox [txt " → ", txt name, txt " - ", txt desc]
    | otherwise = withAttr menuAttr $
        hBox [txt "   ", txt name, txt " - ", txt desc]

-- | Draw stats summary
drawStats :: AppState -> Widget Name
drawStats s = vBox
    [ hBox
        [ withAttr statsAttr $ txt $ T.pack (show dueCount) <> " cards due"
        , txt " | "
        , withAttr correctAttr $ txt $ "Streak: " <> T.pack (show $ asDailyStreak s) <> " days"
        , fill ' '
        , txt "Total: "
        , txt $ T.pack $ show (length $ asVocabCards s)
        , txt " cards"
        ]
    , case asMessage s of
        Just msg -> padTop (Pad 1) $ withAttr hintAttr $ txt msg
        Nothing -> emptyWidget
    ]
  where
    dueCount = length (asDueCards s)

-- | Draw drill screen
drawDrill :: DrillState -> Widget Name
drawDrill drill =
    withBorderStyle unicodeBold $
    vBox
        [ borderWithLabel (withAttr titleAttr $ txt $ " " <> modeLabel <> " ") $
            padAll 2 $ vBox
                [ drawProgress' drill
                , hBorder
                , padTop (Pad 1) $ drawExercise drill
                , padTop (Pad 1) $ drawInput drill
                , padTop (Pad 1) $ drawFeedback drill
                ]
        , hBorder
        , padLeftRight 2 $ drawDrillControls drill
        ]
  where
    modeLabel = case dsMode drill of
        WritingMode -> "Writing Practice"
        ReadingMode -> "Reading Practice"
        TypingMode -> "Typing Practice"

-- | Draw progress bar for drill
drawProgress' :: DrillState -> Widget Name
drawProgress' drill = hBox
    [ txt "Progress: "
    , txt $ T.pack $ show (dsCurrentIndex drill + 1)
    , txt " / "
    , txt $ T.pack $ show (length $ dsExercises drill)
    , fill ' '
    , txt "Score: "
    , withAttr correctAttr $ txt $ T.pack $ show (dsCorrectCount drill)
    , txt " / "
    , txt $ T.pack $ show (dsTotalCount drill)
    ]

-- | Draw current exercise
drawExercise :: DrillState -> Widget Name
drawExercise drill = case currentExercise of
    Nothing -> center $ txt "No more exercises!"
    Just prompt -> center $ vBox
        [ withAttr promptAttr $ txt $ promptLabel (dsMode drill)
        , padTop (Pad 1) $ txtWrap $ epQuestion prompt
        , if dsRevealAnswer drill
            then padTop (Pad 1) $ withAttr hintAttr $ txt $ "Answer: " <> epExpectedAnswer prompt
            else emptyWidget
        ]
  where
    currentExercise = if dsCurrentIndex drill < length (dsExercises drill)
        then Just $ dsExercises drill !! dsCurrentIndex drill
        else Nothing

    promptLabel WritingMode = "Translate to Korean:"
    promptLabel ReadingMode = "What does this mean?"
    promptLabel TypingMode = "Type this in Korean:"

-- | Draw input field
drawInput :: DrillState -> Widget Name
drawInput drill = case dsMode drill of
    ReadingMode -> emptyWidget  -- No input for reading mode
    _ -> center $
        hLimit 50 $
        vLimit 3 $
        withBorderStyle unicode $
        borderWithLabel (txt " Your Answer ") $
        padAll 1 $
        case dsShowResult drill of
            Nothing -> txt $ dsUserInput drill <> "│"
            Just True -> withAttr correctAttr $ txt $ dsUserInput drill
            Just False -> withAttr incorrectAttr $ txt $ dsUserInput drill

-- | Draw feedback after answer
drawFeedback :: DrillState -> Widget Name
drawFeedback drill = case dsShowResult drill of
    Nothing -> emptyWidget
    Just True -> center $ withAttr correctAttr $ txt "✓ Correct! Press Enter to continue"
    Just False -> center $ vBox
        [ withAttr incorrectAttr $ txt "✗ Incorrect"
        , case currentExercise of
            Just prompt -> txt $ "Expected: " <> epExpectedAnswer prompt
            Nothing -> emptyWidget
        , txt "Press Enter to continue"
        ]
  where
    currentExercise = if dsCurrentIndex drill < length (dsExercises drill)
        then Just $ dsExercises drill !! dsCurrentIndex drill
        else Nothing

-- | Draw drill control hints
drawDrillControls :: DrillState -> Widget Name
drawDrillControls drill = case dsMode drill of
    ReadingMode -> hBox
        [ txt "[Space] Reveal Answer | [1-4] Rate (1=Hard, 4=Easy) | [Esc] Exit"
        ]
    _ -> hBox
        [ case dsShowResult drill of
            Nothing -> txt "[Enter] Submit | [Esc] Exit"
            Just _ -> txt "[Enter] Next | [Esc] Exit"
        ]

-- | Draw progress screen
drawProgress :: AppState -> Widget Name
drawProgress s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Your Progress ") $
    padAll 2 $ vBox
        [ -- Streak banner
          center $ withAttr correctAttr $ txt $
              "🔥 " <> T.pack (show $ asDailyStreak s) <> " day streak!"
        , padTop (Pad 1) $ hBorder
        , padTop (Pad 1) $ txt "Mastery Breakdown:"
        , padTop (Pad 1) $ vBox
            [ hBox [ txt "  New:          ", withAttr statsAttr $ txt $ T.pack (show newCount) ]
            , hBox [ txt "  Learning:     ", withAttr hintAttr $ txt $ T.pack (show learningCount) ]
            , hBox [ txt "  Intermediate: ", withAttr promptAttr $ txt $ T.pack (show intermediateCount) ]
            , hBox [ txt "  Mastered:     ", withAttr correctAttr $ txt $ T.pack (show masteredCount) ]
            ]
        , padTop (Pad 1) $ hBorder
        , padTop (Pad 1) $ txt $ "Cards due for review: " <> T.pack (show $ length $ asDueCards s)
        , txt $ "Total vocabulary: " <> T.pack (show $ length $ asVocabCards s)
        , padTop (Pad 2) $ withAttr hintAttr $ txt "Press Esc to return to menu"
        ]
  where
    vocabStates = asVocabStates s
    states = Map.elems vocabStates
    newCount = length $ filter (\vs -> vstMasteryLevel vs == New) states
    learningCount = length $ filter (\vs -> vstMasteryLevel vs == Learning) states
    intermediateCount = length $ filter (\vs -> vstMasteryLevel vs == Intermediate) states
    masteredCount = length $ filter (\vs -> vstMasteryLevel vs == Mastered) states

-- | Draw help screen
drawHelp :: Widget Name
drawHelp =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Help ") $
    padAll 2 $ vBox
        [ txt "Keyboard Shortcuts:"
        , padTop (Pad 1) $ vBox
            [ txt "  ↑/↓ or j/k  - Navigate menu"
            , txt "  Enter       - Select / Submit"
            , txt "  Esc         - Go back / Exit"
            , txt "  ?           - Show this help"
            , txt "  q           - Quit application"
            ]
        , padTop (Pad 2) $ txt "During Writing/Typing Drills:"
        , padTop (Pad 1) $ vBox
            [ txt "  Type        - Enter your answer"
            , txt "  Backspace   - Delete character"
            , txt "  Enter       - Submit answer"
            ]
        , padTop (Pad 2) $ txt "During Reading Drills:"
        , padTop (Pad 1) $ vBox
            [ txt "  Space       - Reveal the translation"
            , txt "  1-4         - Rate difficulty (1=Again, 4=Easy)"
            ]
        , padTop (Pad 2) $ withAttr hintAttr $ txt "Press Esc to return to menu"
        ]

-- | Draw quit confirmation
drawQuitConfirm :: Widget Name
drawQuitConfirm =
    centerLayer $
    withBorderStyle unicodeBold $
    border $
    padAll 2 $ vBox
        [ txt "Are you sure you want to quit?"
        , padTop (Pad 1) $ hBox
            [ txt "[y] Yes  [n] No"
            ]
        ]

-- | Draw typing practice screen with frame
drawTypingPracticeScreen :: TypingState -> Widget Name
drawTypingPracticeScreen ts =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Korean Typing Practice ") $
    padAll 1 $ vBox
        [ drawTypingPractice ts
        , hBorder
        , padTop (Pad 1) $ hCenter $ hBox
            [ txt "[Esc] Exit | [?] Toggle Hints | [Tab] Skip Word"
            ]
        ]

-- | Draw level selector screen with frame
drawLevelSelectScreen :: TypingState -> TypingProgress -> Widget Name
drawLevelSelectScreen ts progress =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Select Typing Level ") $
    padAll 2 $ vBox
        [ center $ drawLevelSelector ts progress
        , hBorder
        , padTop (Pad 1) $ hCenter $ txt "[↑/↓] Navigate | [Enter] Select | [Esc] Back"
        ]

-- | Draw sentence quest screen
drawSentenceQuest :: AppState -> QuestState -> Widget Name
drawSentenceQuest s qs =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Sentence Quest ") $
    vBox
        [ -- Korean keyboard at the top
          padAll 1 $ hCenter $ drawKeyboard keyboardState
        , hBorder
        , padAll 2 $ drawQuestContent qs
        , hBorder
        , padAll 1 $ drawQuestControls qs
        -- Debug message
        , case asMessage s of
            Just msg -> hBorder <=> padAll 1 (withAttr hintAttr $ txt msg)
            Nothing -> emptyWidget
        ]
  where
    -- All keys available, no highlighting for freeform input
    allJamo = Set.unions [allConsonants, allVowels, allDoubleConsonants]
    keyboardState = KeyboardState
        { ksAvailableKeys = allJamo
        , ksNextKey = Nothing
        , ksLastKeyState = Nothing
        }

-- | Draw quest content based on phase
drawQuestContent :: QuestState -> Widget Name
drawQuestContent qs = case questPhase qs of
    EnteringEnglish -> vBox
        [ center $ withAttr promptAttr $ txt "Enter an English sentence to learn:"
        , padTop (Pad 2) $ center $ hLimit 60 $
            withBorderStyle unicode $
            borderWithLabel (txt " English ") $
            padAll 1 $ txt $ getCurrentInput qs <> "│"
        ]

    BuildingKorean -> vBox
        [ -- Show the English sentence
          center $ withAttr promptAttr $ txt "English:"
        , center $ withAttr hintAttr $ txt $ questEnglish qs
        , padTop (Pad 2) $ hBorder
        , padTop (Pad 1) $ center $ txt "Build the Korean translation:"
        , padTop (Pad 1) $ center $ drawChunks (getBuiltChunks qs)
        , padTop (Pad 2) $ center $ hLimit 40 $
            withBorderStyle unicode $
            borderWithLabel (txt " Next word ") $
            padAll 1 $ txt $ getCurrentInput qs <> "│"
        -- Debug: show what getCurrentInput returns
        , padTop (Pad 1) $ center $ txt $ "Input: [" <> getCurrentInput qs <> "]"
        , center $ txt $ "Jamo count: " <> T.pack (show (length (getTypedJamo qs)))
        ]

    Validating -> vBox
        [ center $ withAttr promptAttr $ txt "Review your translation:"
        , padTop (Pad 2) $ center $ vBox
            [ txt "English:"
            , withAttr hintAttr $ txt $ questEnglish qs
            ]
        , padTop (Pad 1) $ center $ vBox
            [ txt "Korean:"
            , withAttr correctAttr $ txt $ T.intercalate " " $ getBuiltChunks qs
            ]
        , padTop (Pad 2) $ center $ withAttr promptAttr $ txt "Is this correct?"
        ]

-- | Draw the built chunks
drawChunks :: [Text] -> Widget Name
drawChunks [] = withAttr chunkPendingAttr $ txt "[...]"
drawChunks chunks = hBox $ map drawChunk chunks ++ [withAttr chunkPendingAttr $ txt " [...]"]
  where
    drawChunk c = withAttr chunkAttr $ txt $ "[" <> c <> "] "

-- | Draw quest controls based on phase
drawQuestControls :: QuestState -> Widget Name
drawQuestControls qs = hCenter $ case questPhase qs of
    EnteringEnglish -> txt "[Enter] Start building Korean | [Esc] Cancel"
    BuildingKorean -> txt "[Enter] Add word | [Space] Finish | [Backspace] Undo last | [Esc] Cancel"
    Validating -> txt "[y] Save | [n] Edit more | [Esc] Cancel"

-- | Draw sentence list screen
drawSentenceList :: AppState -> Widget Name
drawSentenceList s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Your Sentences ") $
    padAll 2 $ vBox
        [ if null (asSentences s)
            then center $ vBox
                [ txt "No sentences yet!"
                , padTop (Pad 1) $ withAttr hintAttr $ txt "Press [n] to create your first sentence"
                ]
            else vBox $ zipWith (drawSentenceItem (asSentenceListIndex s)) [0..] (asSentences s)
        , hBorder
        , padTop (Pad 1) $ hCenter $ txt "[n] New | [Enter] Practice | [v] View versions | [Esc] Back"
        ]

-- | Draw a sentence list item
drawSentenceItem :: Int -> Int -> Sentence -> Widget Name
drawSentenceItem selectedIdx idx sentence
    | selectedIdx == idx = withAttr menuSelectedAttr $ hBox
        [ txt " → "
        , txt $ sentenceEnglish sentence
        , txt " | "
        , txt $ maybe "" svKorean (getLatestVersion sentence)
        ]
    | otherwise = withAttr menuAttr $ hBox
        [ txt "   "
        , txt $ sentenceEnglish sentence
        , txt " | "
        , txt $ maybe "" svKorean (getLatestVersion sentence)
        ]
