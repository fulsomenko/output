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
import Data.List (sortBy, nub, intersperse)
import Data.Maybe (catMaybes)
import Data.Time (localDay)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Output.TUI.Types
import Output.Domain.Exercise (ExercisePrompt(..))
import Output.Domain.Types (TypingProgress, VocabularyState(..), MasteryLevel(..), ExerciseType(..))
import Output.Domain.Activity (ActivityEntry(..), Performance(..), Percentage(..))
import Output.TUI.Widgets.TypingPractice
import qualified Data.Map as Map

-- | Main draw function
drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = case asScreen s of
        MainMenuScreen -> drawMainMenu s
        DrillScreen -> case asDrill s of
            Just drill -> drawDrill s drill
            Nothing -> drawMainMenu s
        TypingPracticeScreen -> case asTyping s of
            Just ts -> drawTypingPracticeScreen s ts
            Nothing -> drawMainMenu s
        TypingLevelSelectScreen -> case asTyping s of
            Just ts -> drawLevelSelectScreen s ts (asTypingProgress s)
            Nothing -> drawMainMenu s
        ProgressScreen -> drawProgress s
        StatsScreen -> drawStatsScreen s
        DayDetailScreen -> drawDayDetail s
        HelpScreen -> drawHelp s
        QuitConfirmScreen -> drawQuitConfirm

-- | Shared status bar shown at the bottom of every screen
statusBar :: AppState -> Text -> Widget Name
statusBar s hints = vBox
    [ hBorder
    , padLeftRight 1 $ hBox
        [ withAttr correctAttr $ txt $ "🔥 " <> T.pack (show $ asDailyStreak s) <> " streak"
        , txt "  ·  "
        , padRight Max $ withAttr statsAttr $ txt $ T.pack (show dueCount) <> " due"
        , withAttr hintAttr $ txt hints
        ]
    ]
  where
    dueCount = length (asDueCards s)

-- | Draw main menu
drawMainMenu :: AppState -> Widget Name
drawMainMenu s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Output - Korean Learning ") $
    vBox
        [ padAll 1 $ drawMenuItems s
        , fill ' '
        , case asMessage s of
            Just msg -> padLeftRight 1 $ withAttr hintAttr $ txt msg
            Nothing  -> emptyWidget
        , statusBar s "[↑/↓] Navigate  [Enter] Select  [q] Quit"
        ]

-- | Draw menu items
drawMenuItems :: AppState -> Widget Name
drawMenuItems s = vBox $ zipWith (drawMenuItem (asMenuIndex s)) [0..] menuOptions
  where
    dueCount = length (asDueCards s)
    dueLabel = "Review Due Cards (" <> T.pack (show dueCount) <> ")"
    menuOptions =
        [ ("Typing Practice", "Learn Korean keyboard with guided levels")
        , (dueLabel,          "Practice words due for SRS review")
        , ("Typing Drill",    "Practice typing Korean words")
        , ("Reading Drill",   "Read Korean and self-grade")
        , ("Writing Drill",   "Translate English to Korean")
        , ("View Progress",   "See your learning statistics")
        , ("Activity Stats",  "View history of all practice sessions")
        , ("Help",            "View keyboard shortcuts")
        , ("Quit",            "Exit the application")
        ]

drawMenuItem :: Int -> Int -> (Text, Text) -> Widget Name
drawMenuItem selected idx (name, desc)
    | selected == idx = withAttr menuSelectedAttr $
        hBox [txt " → ", txt name, txt " - ", txt desc]
    | otherwise = withAttr menuAttr $
        hBox [txt "   ", txt name, txt " - ", txt desc]

-- | Draw drill screen
drawDrill :: AppState -> DrillState -> Widget Name
drawDrill s drill =
    withBorderStyle unicodeBold $
    vBox
        [ borderWithLabel (withAttr titleAttr $ txt $ " " <> modeLabel <> " ") $
            padAll 2 $ vBox
                [ drawProgress' drill
                , hBorder
                , padTop (Pad 1) $ drawExercise drill
                , padTop (Pad 1) $ drawInput drill
                , padTop (Pad 1) $ drawFeedback drill
                , fill ' '
                ]
        , statusBar s drillHints
        ]
  where
    modeLabel = case dsMode drill of
        WritingMode -> "Writing Practice"
        ReadingMode -> "Reading Practice"
        TypingMode  -> "Typing Practice"
    drillHints = case dsMode drill of
        ReadingMode -> "[Space] Reveal  [1-4] Rate  [Esc] Exit"
        _ -> case dsShowResult drill of
            Nothing -> "[Enter] Submit  [Esc] Exit"
            Just _  -> "[Enter] Next  [Esc] Exit"

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
    Nothing -> txt "No more exercises!"
    Just prompt -> vBox
        [ withAttr promptAttr $ txt $ promptLabel (dsMode drill)
        , padTop (Pad 1) $ txtWrap $ epQuestion prompt
        , if dsRevealAnswer drill
            then padTop (Pad 1) $ vBox
                [ withAttr hintAttr $ txt $ "Answer: " <> epExpectedAnswer prompt
                , exampleBlock prompt
                ]
            else emptyWidget
        ]
  where
    currentExercise = if dsCurrentIndex drill < length (dsExercises drill)
        then Just $ dsExercises drill !! dsCurrentIndex drill
        else Nothing
    promptLabel WritingMode = "Translate to Korean:"
    promptLabel ReadingMode = "What does this mean?"
    promptLabel TypingMode  = "Type this in Korean:"
    exampleBlock prompt
        | dsMode drill /= ReadingMode      = emptyWidget
        | null (epExampleSentences prompt) = emptyWidget
        | otherwise = padTop (Pad 1) $ vBox $
            (withAttr promptAttr $ txt "Example:")
            : zipWith renderExample
                (epExampleSentences prompt)
                (epExampleTranslations prompt ++ repeat "")
    renderExample ko en = padLeft (Pad 2) $ vBox
        [ txt ko
        , withAttr hintAttr $ txt en
        ]

-- | Draw input field
drawInput :: DrillState -> Widget Name
drawInput drill = case dsMode drill of
    ReadingMode -> emptyWidget
    _ -> hCenter $
        hLimit 50 $
        vLimit 3 $
        withBorderStyle unicode $
        borderWithLabel (txt " Your Answer ") $
        padAll 1 $
        case dsShowResult drill of
            Nothing   -> txt $ dsUserInput drill <> "│"
            Just True -> withAttr correctAttr  $ txt $ dsUserInput drill
            Just False -> withAttr incorrectAttr $ txt $ dsUserInput drill

-- | Draw feedback after answer
drawFeedback :: DrillState -> Widget Name
drawFeedback drill = case dsShowResult drill of
    Nothing    -> emptyWidget
    Just True  -> withAttr correctAttr $ txt "✓ Correct! Press Enter to continue"
    Just False -> vBox
        [ withAttr incorrectAttr $ txt "✗ Incorrect"
        , case currentExercise of
            Just prompt -> txt $ "Expected: " <> epExpectedAnswer prompt
            Nothing     -> emptyWidget
        , txt "Press Enter to continue"
        ]
  where
    currentExercise = if dsCurrentIndex drill < length (dsExercises drill)
        then Just $ dsExercises drill !! dsCurrentIndex drill
        else Nothing

-- | Draw progress screen
drawProgress :: AppState -> Widget Name
drawProgress s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Your Progress ") $
    padAll 2 $ vBox
        [ txt "Mastery Breakdown:"
        , padTop (Pad 1) $ vBox
            [ hBox [ txt "  New:          ", withAttr statsAttr  $ txt $ T.pack (show newCount) ]
            , hBox [ txt "  Learning:     ", withAttr hintAttr   $ txt $ T.pack (show learningCount) ]
            , hBox [ txt "  Intermediate: ", withAttr promptAttr $ txt $ T.pack (show intermediateCount) ]
            , hBox [ txt "  Mastered:     ", withAttr correctAttr $ txt $ T.pack (show masteredCount) ]
            ]
        , padTop (Pad 1) $ hBorder
        , padTop (Pad 1) $ txt $ "Cards due for review: " <> T.pack (show $ length $ asDueCards s)
        , txt $ "Total vocabulary: " <> T.pack (show $ length $ asVocabCards s)
        , fill ' '
        , statusBar s "[Esc] Back"
        ]
  where
    states = Map.elems (asVocabStates s)
    newCount          = length $ filter (\vs -> vstMasteryLevel vs == New)          states
    learningCount     = length $ filter (\vs -> vstMasteryLevel vs == Learning)     states
    intermediateCount = length $ filter (\vs -> vstMasteryLevel vs == Intermediate) states
    masteredCount     = length $ filter (\vs -> vstMasteryLevel vs == Mastered)     states

-- | Draw activity stats screen
drawStatsScreen :: AppState -> Widget Name
drawStatsScreen s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Activity Stats ") $
    padAll 2 $ vBox
        [ if null activities
            then withAttr hintAttr $ txt "No sessions recorded yet — go practice!"
            else vBox $ zipWith (drawDayRow sel) [0..] dayGroups
        , fill ' '
        , statusBar s "[↑/↓] Navigate  [Enter] Details  [Esc] Back"
        ]
  where
    activities = asActivities s
    dayGroups  = groupActivitiesByDay activities
    sel        = asStatsSelectedDay s

groupActivitiesByDay :: [ActivityEntry] -> [(Day, [ActivityEntry])]
groupActivitiesByDay entries =
    let days = sortBy (\a b -> compare b a) $ nub $ map (localDay . actDate) entries
    in  [(d, filter (\e -> localDay (actDate e) == d) entries) | d <- days]

-- | Draw a generic row with consistent spacing
drawRow :: Bool -> [Widget Name] -> [Widget Name] -> [Widget Name] -> Widget Name
drawRow isSelected left middle right =
    applyIf isSelected (withAttr menuSelectedAttr) $
    hBox $ left ++ [txt "  "] ++ intersperse (txt "  ") middle ++ [padRight Max emptyWidget, txt "  "] ++ right

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True  f x = f x
applyIf False _ x = x

drawDayRow :: Int -> Int -> (Day, [ActivityEntry]) -> Widget Name
drawDayRow selected idx (day, entries) =
    drawRow (selected == idx) [txt dateStr] summaries [txt ""]
  where
    dateStr   = T.pack $ formatTime defaultTimeLocale "%b %d" day
    summaries = catMaybes
        [ drillSummary "Typing"  Typing  entries
        , drillSummary "Reading" Reading entries
        , drillSummary "Writing" Writing entries
        ]

drillSummary :: Text -> ExerciseType -> [ActivityEntry] -> Maybe (Widget Name)
drillSummary label exType entries
    | null group = Nothing
    | otherwise  = Just $ hBox
        [ withAttr promptAttr $ txt $ label <> " \xd7" <> T.pack (show count)
        , txt "  "
        , withAttr accAttr $ txt avgStr
        ]
  where
    group   = filter (\a -> actExerciseType a == exType) entries
    count   = length group
    total   = sum [ v | e <- group, let Percentage v = perfAccuracy (actPerformance e) ]
    avgAcc  = total / fromIntegral count
    avgStr  = T.pack (show (round avgAcc :: Int)) <> "%"
    anySucc = any actSuccess group
    accAttr = if anySucc then correctAttr else hintAttr

-- | Average accuracy across all entries
avgAccuracy :: [ActivityEntry] -> Maybe Double
avgAccuracy [] = Nothing
avgAccuracy es = Just $ total / fromIntegral (length es)
  where total = sum [ v | e <- es, let Percentage v = perfAccuracy (actPerformance e) ]

-- | Day summary: type breakdown, sessions count, avg accuracy, pass count, trend
drawDaySummary :: [ActivityEntry] -> Widget Name
drawDaySummary entries = hBox
    [ hBox $ intersperse (txt "   ") typeSummaries
    , padLeft Max $ hBox
        [ txt $ T.pack (show (length entries)) <> " sessions"
        , txt "  ·  Avg "
        , withAttr accAttr $ txt avgStr
        , txt "  ·  "
        , withAttr correctAttr $ txt $ T.pack (show passCount) <> " passed"
        , txt "  "
        , withAttr trendAttr $ txt trendStr
        ]
    ]
  where
    typeSummaries = catMaybes
        [ drillSummary "Typing"  Typing  entries
        , drillSummary "Reading" Reading entries
        , drillSummary "Writing" Writing entries
        ]
    passCount = length $ filter actSuccess entries
    avg       = avgAccuracy entries
    avgStr    = maybe "—" (\v -> T.pack (show (round v :: Int)) <> "%") avg
    accAttr   = if any actSuccess entries then correctAttr else hintAttr
    sorted    = sortBy (\a b -> compare (actDate a) (actDate b)) entries
    half      = length sorted `div` 2
    (firstH, secondH) = splitAt (max 1 half) sorted
    trendStr  = case (avgAccuracy firstH, avgAccuracy secondH) of
        (Just f, Just s)
            | s > f + 2  -> "↑ trend"
            | s < f - 2  -> "↓ trend"
        _                -> ""
    trendAttr = case (avgAccuracy firstH, avgAccuracy secondH) of
        (Just f, Just s) | s > f + 2 -> correctAttr
                         | s < f - 2 -> incorrectAttr
        _                             -> hintAttr

-- | Day comparisons: vs previous practice day and vs 7-day average
drawDayComparisons :: [(Day, [ActivityEntry])] -> Int -> Day -> Widget Name
drawDayComparisons groups idx today = hBox $ intersperse (txt "   ·   ") comparisons
  where
    todayAvg = avgAccuracy $ concatMap snd $ take 1 $ drop idx groups

    -- Previous practice day
    prevComparison = case drop (idx + 1) groups of
        ((prevDay, prevEntries) : _) ->
            case (todayAvg, avgAccuracy prevEntries) of
                (Just t, Just p) ->
                    let delta = round (t - p) :: Int
                        sign  = if delta >= 0 then "↑ +" else "↓ "
                        label = T.pack $ formatTime defaultTimeLocale "%b %d" prevDay
                        attr  = if delta >= 0 then correctAttr else incorrectAttr
                    in  Just $ hBox [ withAttr attr $ txt $ sign <> T.pack (show (abs delta)) <> "%"
                                    , withAttr hintAttr $ txt $ " vs. " <> label
                                    , txt $ " (" <> T.pack (show (round p :: Int)) <> "%)"
                                    ]
                _ -> Nothing
        _ -> Nothing

    -- 7-day rolling average
    sevenDayEntries = [ e | (d, es) <- groups
                          , diffDays today d >= 1 && diffDays today d <= 7
                          , e <- es ]
    weekComparison = case (todayAvg, avgAccuracy sevenDayEntries) of
        (Just t, Just w) ->
            let delta = round (t - w) :: Int
                sign  = if delta >= 0 then "↑ +" else "↓ "
                attr  = if delta >= 0 then correctAttr else incorrectAttr
            in  Just $ hBox [ withAttr attr $ txt $ sign <> T.pack (show (abs delta)) <> "%"
                            , withAttr hintAttr $ txt " vs. 7-day avg"
                            , txt $ " (" <> T.pack (show (round w :: Int)) <> "%)"
                            ]
        _ -> Nothing

    comparisons = catMaybes [prevComparison, weekComparison]

-- | Draw day detail screen
drawDayDetail :: AppState -> Widget Name
drawDayDetail s =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt $ " " <> dateHeader <> " ") $
    padAll 2 $ vBox
        [ drawDaySummary dayEntries
        , padTop (Pad 1) $ drawDayComparisons groups (asStatsSelectedDay s) day
        , padTop (Pad 1) hBorder
        , padTop (Pad 1) $ vBox $ map drawDetailRow dayEntries
        , fill ' '
        , statusBar s "[Esc] Back"
        ]
  where
    groups = groupActivitiesByDay (asActivities s)
    (day, dayEntries) = case drop (asStatsSelectedDay s) groups of
        (x : _) -> x
        []      -> (toEnum 0, [])
    dateHeader = T.pack $ formatTime defaultTimeLocale "%B %d, %Y" day

drawDetailRow :: ActivityEntry -> Widget Name
drawDetailRow entry =
    drawRow False
        [withAttr hintAttr $ txt timeStr]
        ([ withAttr promptAttr $ txt typeStr, txt notesStr ] ++ wpmWidget)
        [withAttr accAttr $ txt accStr, withAttr succAttr $ txt succStr]
  where
    timeStr  = T.pack $ formatTime defaultTimeLocale "%H:%M" (actDate entry)
    typeStr  = case actExerciseType entry of
        Typing  -> "Typing "
        Reading -> "Reading"
        Writing -> "Writing"
    notesStr = maybe "—" id (actNotes entry)
    Percentage accVal = perfAccuracy (actPerformance entry)
    accStr   = T.pack (show (round accVal :: Int)) <> "%"
    succStr  = if actSuccess entry then "✓" else " "
    accAttr  = if actSuccess entry then correctAttr else hintAttr
    succAttr = if actSuccess entry then correctAttr else hintAttr
    wpmWidget = case perfWpm (actPerformance entry) of
        Just w  -> [withAttr statsAttr $ txt $ T.pack (show w) <> "wpm"]
        Nothing -> []

-- | Draw help screen
drawHelp :: AppState -> Widget Name
drawHelp s =
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
        , fill ' '
        , statusBar s "[Esc] Back"
        ]

-- | Draw quit confirmation
drawQuitConfirm :: Widget Name
drawQuitConfirm =
    centerLayer $
    withBorderStyle unicodeBold $
    border $
    padAll 2 $ vBox
        [ txt "Are you sure you want to quit?"
        , padTop (Pad 1) $ txt "[Y] Quit  [n] Cancel"
        ]

-- | Draw typing practice screen with frame
drawTypingPracticeScreen :: AppState -> TypingState -> Widget Name
drawTypingPracticeScreen s ts =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Korean Typing Practice ") $
    padAll 1 $ vBox
        [ drawTypingPractice ts
        , statusBar s "[Esc] Exit  [?] Toggle Hints  [Tab] Skip Word"
        ]

-- | Draw level selector screen with frame
drawLevelSelectScreen :: AppState -> TypingState -> TypingProgress -> Widget Name
drawLevelSelectScreen s ts progress =
    withBorderStyle unicodeBold $
    borderWithLabel (withAttr titleAttr $ txt " Select Typing Level ") $
    padAll 2 $ vBox
        [ drawLevelSelector ts progress
        , fill ' '
        , statusBar s "[↑/↓] Navigate  [Enter] Select  [Esc] Back"
        ]
