{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Widgets.Keyboard
    ( -- * Types
      KeyState(..)
    , KeyboardState(..)
      -- * Drawing
    , drawKeyboard
    , drawKeyboardCompact
      -- * Key Info
    , fingerColor
    , keyRow
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Output.Domain.Jamo

-- | State of a single key
data KeyState
    = KeyNormal       -- Default state
    | KeyHighlighted  -- Next key to press
    | KeyCorrect      -- Just pressed correctly (flash green)
    | KeyError        -- Just pressed incorrectly (flash red)
    | KeyDisabled     -- Not yet unlocked (dimmed)
    deriving (Eq, Show)

-- | Full keyboard state
data KeyboardState = KeyboardState
    { ksAvailableKeys :: Set Jamo    -- Keys that are unlocked
    , ksNextKey :: Maybe Jamo        -- Key to highlight
    , ksLastKeyState :: Maybe (Jamo, KeyState)  -- Last pressed key and its state
    } deriving (Eq, Show)

-- | Get color for a finger position (0-9 from pinky to pinky)
fingerColor :: Int -> V.Color
fingerColor 0 = V.red       -- Left pinky
fingerColor 1 = V.yellow    -- Left ring
fingerColor 2 = V.green     -- Left middle
fingerColor 3 = V.cyan      -- Left index
fingerColor 4 = V.cyan      -- Left index (stretch)
fingerColor 5 = V.magenta   -- Right index (stretch)
fingerColor 6 = V.magenta   -- Right index
fingerColor 7 = V.green     -- Right middle
fingerColor 8 = V.yellow    -- Right ring
fingerColor 9 = V.red       -- Right pinky
fingerColor _ = V.white

-- | Get which row a jamo belongs to (0=top, 1=home, 2=bottom)
keyRow :: Jamo -> Int
keyRow j
    | j `Set.member` topRowJamo = 0
    | j `Set.member` homeRowJamo = 1
    | j `Set.member` bottomRowJamo = 2
    | otherwise = 1  -- Default to home row

-- | QWERTY keyboard layout - top row jamo
topRowLayout :: [(Char, Jamo)]
topRowLayout =
    [ ('q', Consonant 'ㅂ'), ('w', Consonant 'ㅈ'), ('e', Consonant 'ㄷ')
    , ('r', Consonant 'ㄱ'), ('t', Consonant 'ㅅ'), ('y', Vowel 'ㅛ')
    , ('u', Vowel 'ㅕ'), ('i', Vowel 'ㅑ'), ('o', Vowel 'ㅐ'), ('p', Vowel 'ㅔ')
    ]

-- | QWERTY keyboard layout - home row jamo
homeRowLayout :: [(Char, Jamo)]
homeRowLayout =
    [ ('a', Consonant 'ㅁ'), ('s', Consonant 'ㄴ'), ('d', Consonant 'ㅇ')
    , ('f', Consonant 'ㄹ'), ('g', Consonant 'ㅎ'), ('h', Vowel 'ㅗ')
    , ('j', Vowel 'ㅓ'), ('k', Vowel 'ㅏ'), ('l', Vowel 'ㅣ')
    ]

-- | QWERTY keyboard layout - bottom row jamo
bottomRowLayout :: [(Char, Jamo)]
bottomRowLayout =
    [ ('z', Consonant 'ㅋ'), ('x', Consonant 'ㅌ'), ('c', Consonant 'ㅊ')
    , ('v', Consonant 'ㅍ'), ('b', Vowel 'ㅠ'), ('n', Vowel 'ㅜ'), ('m', Vowel 'ㅡ')
    ]

-- | Finger index for each position in a row
topRowFingers :: [Int]
topRowFingers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

homeRowFingers :: [Int]
homeRowFingers = [0, 1, 2, 3, 4, 5, 6, 7, 8]

bottomRowFingers :: [Int]
bottomRowFingers = [0, 1, 2, 3, 4, 5, 6]

-- | Draw a single key
drawKey :: KeyboardState -> Int -> (Char, Jamo) -> Widget n
drawKey kbState fingerIdx (qwerty, jamo) =
    modifyDefAttr (const attr) $
        padLeftRight 1 $
            txt (T.singleton $ jamoChar jamo)
  where
    attr = case keyState of
        KeyDisabled -> V.defAttr `V.withForeColor` V.brightBlack
        KeyNormal -> V.defAttr `V.withForeColor` fingerColor fingerIdx
        KeyHighlighted -> V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.white
            `V.withStyle` V.bold
        KeyCorrect -> V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.green
            `V.withStyle` V.bold
        KeyError -> V.defAttr
            `V.withForeColor` V.white
            `V.withBackColor` V.red
            `V.withStyle` V.bold

    keyState
        -- Check if this was the last pressed key
        | Just (lastJamo, lastState) <- ksLastKeyState kbState
        , lastJamo == jamo
        = lastState
        -- Check if this is the next expected key
        | Just nextJamo <- ksNextKey kbState
        , nextJamo == jamo
        = KeyHighlighted
        -- Check if key is available
        | not (jamo `Set.member` ksAvailableKeys kbState)
        = KeyDisabled
        -- Default state
        | otherwise
        = KeyNormal

-- | Draw a row of keys
drawKeyRow :: KeyboardState -> [(Char, Jamo)] -> [Int] -> Widget n
drawKeyRow kbState layout fingers =
    hBox $ zipWith (drawKey kbState) fingers layout

-- | Draw the full keyboard
drawKeyboard :: KeyboardState -> Widget n
drawKeyboard kbState =
    borderWithLabel (txt " Keyboard ") $
        padAll 1 $
            vBox
                [ hCenter $ drawKeyRow kbState topRowLayout topRowFingers
                , hCenter $ padLeft (Pad 1) $ drawKeyRow kbState homeRowLayout homeRowFingers
                , hCenter $ padLeft (Pad 2) $ drawKeyRow kbState bottomRowLayout bottomRowFingers
                ]

-- | Draw a compact keyboard (single line showing available keys)
drawKeyboardCompact :: KeyboardState -> Widget n
drawKeyboardCompact kbState =
    hBox
        [ txt "Keys: "
        , hBox $ map (drawCompactKey kbState) allLayoutKeys
        ]
  where
    allLayoutKeys = homeRowLayout ++ topRowLayout ++ bottomRowLayout

-- | Draw a single compact key
drawCompactKey :: KeyboardState -> (Char, Jamo) -> Widget n
drawCompactKey kbState (_, jamo)
    | not (jamo `Set.member` ksAvailableKeys kbState) = emptyWidget
    | otherwise = case ksNextKey kbState of
        Just nextJamo | nextJamo == jamo ->
            withAttr highlightAttr $ txt (T.singleton $ jamoChar jamo)
        _ -> txt (T.singleton $ jamoChar jamo)
  where
    highlightAttr = attrName "highlight"

-- | Get the character from a Jamo
jamoChar :: Jamo -> Char
jamoChar (Consonant c) = c
jamoChar (Vowel c) = c
jamoChar (DoubleConsonant c) = c
