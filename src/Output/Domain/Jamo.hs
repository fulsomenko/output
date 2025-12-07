{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Jamo
    ( -- * Jamo Types
      Jamo(..)
    , JamoType(..)
    , getJamoType
    , getJamoChar
      -- * QWERTY Mapping (2-beolsik)
    , qwertyToJamo
    , jamoToQwerty
    , shiftQwertyToJamo
      -- * Korean Decomposition/Composition
    , decomposeKorean
    , composeSyllable
    , composeJamoSequence
    , isKoreanChar
    , isJamoChar
    , charToJamo
      -- * Key Position
    , KeyRow(..)
    , getKeyRow
    , isHomeRow
    , isTopRow
    , isBottomRow
      -- * Display Helpers
    , jamoName
    , jamoSound
      -- * All Jamo Sets
    , allConsonants
    , allVowels
    , allDoubleConsonants
    , homeRowJamo
    , topRowJamo
    , bottomRowJamo
    ) where

import Data.Char (ord, chr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Jamo (Korean letter) types
data Jamo
    = Consonant Char        -- ㄱ ㄴ ㄷ ㄹ ㅁ ㅂ ㅅ ㅇ ㅈ ㅊ ㅋ ㅌ ㅍ ㅎ
    | Vowel Char            -- ㅏ ㅐ ㅑ ㅒ ㅓ ㅔ ㅕ ㅖ ㅗ ㅘ ㅙ ㅚ ㅛ ㅜ ㅝ ㅞ ㅟ ㅠ ㅡ ㅢ ㅣ
    | DoubleConsonant Char  -- ㄲ ㄸ ㅃ ㅆ ㅉ
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Jamo
instance ToJSON Jamo

-- | Classification of jamo
data JamoType = ConsonantType | VowelType | DoubleConsonantType
    deriving (Eq, Show)

getJamoType :: Jamo -> JamoType
getJamoType (Consonant _) = ConsonantType
getJamoType (Vowel _) = VowelType
getJamoType (DoubleConsonant _) = DoubleConsonantType

getJamoChar :: Jamo -> Char
getJamoChar (Consonant c) = c
getJamoChar (Vowel c) = c
getJamoChar (DoubleConsonant c) = c

-- | Keyboard row
data KeyRow = TopRow | HomeRow | BottomRow
    deriving (Eq, Show)

-- | 2-beolsik QWERTY to Jamo mapping (lowercase)
qwertyMap :: Map Char Jamo
qwertyMap = Map.fromList
    -- Top row: q w e r t y u i o p
    [ ('q', Consonant 'ㅂ')
    , ('w', Consonant 'ㅈ')
    , ('e', Consonant 'ㄷ')
    , ('r', Consonant 'ㄱ')
    , ('t', Consonant 'ㅅ')
    , ('y', Vowel 'ㅛ')
    , ('u', Vowel 'ㅕ')
    , ('i', Vowel 'ㅑ')
    , ('o', Vowel 'ㅐ')
    , ('p', Vowel 'ㅔ')
    -- Home row: a s d f g h j k l
    , ('a', Consonant 'ㅁ')
    , ('s', Consonant 'ㄴ')
    , ('d', Consonant 'ㅇ')
    , ('f', Consonant 'ㄹ')
    , ('g', Consonant 'ㅎ')
    , ('h', Vowel 'ㅗ')
    , ('j', Vowel 'ㅓ')
    , ('k', Vowel 'ㅏ')
    , ('l', Vowel 'ㅣ')
    -- Bottom row: z x c v b n m
    , ('z', Consonant 'ㅋ')
    , ('x', Consonant 'ㅌ')
    , ('c', Consonant 'ㅊ')
    , ('v', Consonant 'ㅍ')
    , ('b', Vowel 'ㅠ')
    , ('n', Vowel 'ㅜ')
    , ('m', Vowel 'ㅡ')
    ]

-- | Shift + QWERTY for double consonants
shiftQwertyMap :: Map Char Jamo
shiftQwertyMap = Map.fromList
    [ ('Q', DoubleConsonant 'ㅃ')
    , ('W', DoubleConsonant 'ㅉ')
    , ('E', DoubleConsonant 'ㄸ')
    , ('R', DoubleConsonant 'ㄲ')
    , ('T', DoubleConsonant 'ㅆ')
    -- Shift vowels (same as lowercase for compound vowels)
    , ('O', Vowel 'ㅒ')
    , ('P', Vowel 'ㅖ')
    ]

-- | Reverse mapping: Jamo to QWERTY key
jamoToQwertyMap :: Map Jamo Char
jamoToQwertyMap = Map.fromList $
    [(j, k) | (k, j) <- Map.toList qwertyMap] ++
    [(j, k) | (k, j) <- Map.toList shiftQwertyMap]

-- | Convert QWERTY key to Jamo
qwertyToJamo :: Char -> Maybe Jamo
qwertyToJamo c = Map.lookup c qwertyMap

-- | Convert shift+QWERTY key to Jamo (for double consonants)
shiftQwertyToJamo :: Char -> Maybe Jamo
shiftQwertyToJamo c = Map.lookup c shiftQwertyMap

-- | Convert Jamo to QWERTY key
jamoToQwerty :: Jamo -> Maybe Char
jamoToQwerty j = Map.lookup j jamoToQwertyMap

-- | Get keyboard row for a jamo
getKeyRow :: Jamo -> Maybe KeyRow
getKeyRow j = case jamoToQwerty j of
    Nothing -> Nothing
    Just c
        | c `elem` ("qwertyuiop" :: String) -> Just TopRow
        | c `elem` ("asdfghjkl" :: String) -> Just HomeRow
        | c `elem` ("zxcvbnm" :: String) -> Just BottomRow
        | c `elem` ("QWERT" :: String) -> Just TopRow  -- Shift keys
        | otherwise -> Nothing

isHomeRow :: Jamo -> Bool
isHomeRow j = getKeyRow j == Just HomeRow

isTopRow :: Jamo -> Bool
isTopRow j = getKeyRow j == Just TopRow

isBottomRow :: Jamo -> Bool
isBottomRow j = getKeyRow j == Just BottomRow

-- | Check if character is a Korean syllable (가-힣)
isKoreanChar :: Char -> Bool
isKoreanChar c = ord c >= 0xAC00 && ord c <= 0xD7A3

-- | Check if character is a Hangul Compatibility Jamo (ㄱ-ㅣ, U+3131-U+3163)
isJamoChar :: Char -> Bool
isJamoChar c = ord c >= 0x3131 && ord c <= 0x3163

-- | Convert a raw jamo character to a Jamo type
charToJamo :: Char -> Jamo
charToJamo c
    | c `elem` ("ㄲㄸㅃㅆㅉ" :: String) = DoubleConsonant c
    | c `elem` ("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ" :: String) = Vowel c
    | otherwise = Consonant c

-- | Initial consonant index table
initialConsonants :: [Char]
initialConsonants = ['ㄱ','ㄲ','ㄴ','ㄷ','ㄸ','ㄹ','ㅁ','ㅂ','ㅃ','ㅅ','ㅆ','ㅇ','ㅈ','ㅉ','ㅊ','ㅋ','ㅌ','ㅍ','ㅎ']

-- | Medial vowel index table
medialVowels :: [Char]
medialVowels = ['ㅏ','ㅐ','ㅑ','ㅒ','ㅓ','ㅔ','ㅕ','ㅖ','ㅗ','ㅘ','ㅙ','ㅚ','ㅛ','ㅜ','ㅝ','ㅞ','ㅟ','ㅠ','ㅡ','ㅢ','ㅣ']

-- | Final consonant index table (0 = no final)
finalConsonants :: [Char]
finalConsonants = [' ','ㄱ','ㄲ','ㄳ','ㄴ','ㄵ','ㄶ','ㄷ','ㄹ','ㄺ','ㄻ','ㄼ','ㄽ','ㄾ','ㄿ','ㅀ','ㅁ','ㅂ','ㅄ','ㅅ','ㅆ','ㅇ','ㅈ','ㅊ','ㅋ','ㅌ','ㅍ','ㅎ']

-- | Decompose a Korean syllable into jamo
decomposeSyllable :: Char -> Maybe [Jamo]
decomposeSyllable c
    | not (isKoreanChar c) = Nothing
    | otherwise =
        let code = ord c - 0xAC00
            initialIdx = code `div` 588
            medialIdx = (code `mod` 588) `div` 28
            finalIdx = code `mod` 28
            initial = initialConsonants !! initialIdx
            medial = medialVowels !! medialIdx
            mFinal = if finalIdx == 0 then Nothing else Just (finalConsonants !! finalIdx)
        in Just $ [toJamo initial, Vowel medial] ++ maybe [] (\f -> [toJamo f]) mFinal
  where
    toJamo c'
        | c' `elem` ("ㄲㄸㅃㅆㅉ" :: String) = DoubleConsonant c'
        | c' `elem` ("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ" :: String) = Vowel c'
        | otherwise = Consonant c'

-- | Decompose Korean text into jamo sequence
-- Handles both composed syllables (가-힣) and raw jamo (ㄱ-ㅣ)
decomposeKorean :: Text -> [Jamo]
decomposeKorean = concatMap decomposeChar . T.unpack
  where
    decomposeChar c
        | isKoreanChar c = case decomposeSyllable c of
            Just jamos -> jamos
            Nothing -> []
        | isJamoChar c = [charToJamo c]  -- Handle raw jamo
        | otherwise = []  -- Skip non-Korean characters

-- | Compose jamo into a syllable (simplified: initial + medial + optional final)
composeSyllable :: Jamo -> Jamo -> Maybe Jamo -> Maybe Char
composeSyllable initial medial mFinal = do
    initialIdx <- findIndex (getJamoChar initial) initialConsonants
    medialIdx <- findIndex (getJamoChar medial) medialVowels
    let finalIdx = case mFinal of
            Nothing -> 0
            Just f -> maybe 0 id (findIndex (getJamoChar f) finalConsonants)
    let code = 0xAC00 + (initialIdx * 588) + (medialIdx * 28) + finalIdx
    Just $ chr code
  where
    findIndex x xs = lookup x (zip xs [0..])

-- | State for syllable composition
data ComposeState
    = Empty                           -- No pending input
    | HasInitial Jamo                 -- Have initial consonant
    | HasSyllable Jamo Jamo           -- Have initial + medial (complete syllable)
    | HasFinal Jamo Jamo Jamo         -- Have initial + medial + final
    deriving (Show, Eq)

-- | Check if a jamo can be a valid final consonant (batchim)
canBeFinal :: Jamo -> Bool
canBeFinal j = getJamoChar j `elem` ("ㄱㄲㄳㄴㄵㄶㄷㄹㄺㄻㄼㄽㄾㄿㅀㅁㅂㅄㅅㅆㅇㅈㅊㅋㅌㅍㅎ" :: String)

-- | Check if jamo is a consonant (initial or final position)
isConsonant :: Jamo -> Bool
isConsonant (Consonant _) = True
isConsonant (DoubleConsonant _) = True
isConsonant (Vowel _) = False

-- | Check if jamo is a vowel
isVowel :: Jamo -> Bool
isVowel (Vowel _) = True
isVowel _ = False

-- | Render a syllable state to text
renderState :: ComposeState -> Text
renderState Empty = T.empty
renderState (HasInitial j) = T.singleton (getJamoChar j)
renderState (HasSyllable initial medial) =
    case composeSyllable initial medial Nothing of
        Just c -> T.singleton c
        Nothing -> T.pack [getJamoChar initial, getJamoChar medial]
renderState (HasFinal initial medial final) =
    case composeSyllable initial medial (Just final) of
        Just c -> T.singleton c
        Nothing -> T.pack [getJamoChar initial, getJamoChar medial, getJamoChar final]

-- | Compose a sequence of jamo into Korean text (like a Korean IME)
-- This handles real-time composition: ㅁ→ㅁ, ㅁㅏ→마, ㅁㅏㄴ→만, ㅁㅏㄴㅏ→마나
composeJamoSequence :: [Jamo] -> Text
composeJamoSequence = go Empty T.empty
  where
    go :: ComposeState -> Text -> [Jamo] -> Text
    go state acc [] = acc <> renderState state
    go state acc (j:js) = case (state, j) of
        -- Empty state
        (Empty, _) | isConsonant j -> go (HasInitial j) acc js
        (Empty, _) | isVowel j -> go Empty (acc <> T.singleton (getJamoChar j)) js

        -- Has initial consonant
        (HasInitial initial, _) | isVowel j ->
            go (HasSyllable initial j) acc js
        (HasInitial initial, _) | isConsonant j ->
            go (HasInitial j) (acc <> T.singleton (getJamoChar initial)) js

        -- Has complete syllable (initial + medial)
        (HasSyllable initial medial, _) | isConsonant j && canBeFinal j ->
            go (HasFinal initial medial j) acc js
        (HasSyllable initial medial, _) | isConsonant j ->
            -- This consonant can't be a final, emit syllable and start new
            go (HasInitial j) (acc <> renderState state) js
        (HasSyllable initial medial, _) | isVowel j ->
            -- Vowel after complete syllable - emit and this vowel stands alone
            go Empty (acc <> renderState state <> T.singleton (getJamoChar j)) js

        -- Has syllable with final consonant
        (HasFinal initial medial final, _) | isVowel j ->
            -- Vowel after final: final becomes initial of new syllable
            let completeSyllable = case composeSyllable initial medial Nothing of
                    Just c -> T.singleton c
                    Nothing -> T.pack [getJamoChar initial, getJamoChar medial]
            in go (HasSyllable final j) (acc <> completeSyllable) js
        (HasFinal initial medial final, _) | isConsonant j ->
            -- Another consonant: emit current syllable, new consonant is initial
            go (HasInitial j) (acc <> renderState state) js

        -- Fallback
        _ -> go state (acc <> T.singleton (getJamoChar j)) js

-- | Get display name for jamo
jamoName :: Jamo -> Text
jamoName j = case getJamoChar j of
    'ㄱ' -> "giyeok"
    'ㄲ' -> "ssang-giyeok"
    'ㄴ' -> "nieun"
    'ㄷ' -> "digeut"
    'ㄸ' -> "ssang-digeut"
    'ㄹ' -> "rieul"
    'ㅁ' -> "mieum"
    'ㅂ' -> "bieup"
    'ㅃ' -> "ssang-bieup"
    'ㅅ' -> "siot"
    'ㅆ' -> "ssang-siot"
    'ㅇ' -> "ieung"
    'ㅈ' -> "jieut"
    'ㅉ' -> "ssang-jieut"
    'ㅊ' -> "chieut"
    'ㅋ' -> "kieuk"
    'ㅌ' -> "tieut"
    'ㅍ' -> "pieup"
    'ㅎ' -> "hieut"
    'ㅏ' -> "a"
    'ㅐ' -> "ae"
    'ㅑ' -> "ya"
    'ㅒ' -> "yae"
    'ㅓ' -> "eo"
    'ㅔ' -> "e"
    'ㅕ' -> "yeo"
    'ㅖ' -> "ye"
    'ㅗ' -> "o"
    'ㅘ' -> "wa"
    'ㅙ' -> "wae"
    'ㅚ' -> "oe"
    'ㅛ' -> "yo"
    'ㅜ' -> "u"
    'ㅝ' -> "wo"
    'ㅞ' -> "we"
    'ㅟ' -> "wi"
    'ㅠ' -> "yu"
    'ㅡ' -> "eu"
    'ㅢ' -> "ui"
    'ㅣ' -> "i"
    _ -> T.singleton (getJamoChar j)

-- | Get romanized sound for jamo
jamoSound :: Jamo -> Text
jamoSound j = case getJamoChar j of
    'ㄱ' -> "g/k"
    'ㄲ' -> "kk"
    'ㄴ' -> "n"
    'ㄷ' -> "d/t"
    'ㄸ' -> "tt"
    'ㄹ' -> "r/l"
    'ㅁ' -> "m"
    'ㅂ' -> "b/p"
    'ㅃ' -> "pp"
    'ㅅ' -> "s"
    'ㅆ' -> "ss"
    'ㅇ' -> "ng/∅"
    'ㅈ' -> "j"
    'ㅉ' -> "jj"
    'ㅊ' -> "ch"
    'ㅋ' -> "k"
    'ㅌ' -> "t"
    'ㅍ' -> "p"
    'ㅎ' -> "h"
    'ㅏ' -> "ah"
    'ㅐ' -> "ae"
    'ㅑ' -> "yah"
    'ㅒ' -> "yae"
    'ㅓ' -> "uh"
    'ㅔ' -> "eh"
    'ㅕ' -> "yuh"
    'ㅖ' -> "yeh"
    'ㅗ' -> "oh"
    'ㅘ' -> "wah"
    'ㅙ' -> "wae"
    'ㅚ' -> "weh"
    'ㅛ' -> "yoh"
    'ㅜ' -> "oo"
    'ㅝ' -> "wuh"
    'ㅞ' -> "weh"
    'ㅟ' -> "wee"
    'ㅠ' -> "yoo"
    'ㅡ' -> "eu"
    'ㅢ' -> "eui"
    'ㅣ' -> "ee"
    _ -> T.singleton (getJamoChar j)

-- | All basic consonants
allConsonants :: Set Jamo
allConsonants = Set.fromList $ map Consonant "ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ"

-- | All vowels
allVowels :: Set Jamo
allVowels = Set.fromList $ map Vowel "ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ"

-- | All double consonants
allDoubleConsonants :: Set Jamo
allDoubleConsonants = Set.fromList $ map DoubleConsonant "ㄲㄸㅃㅆㅉ"

-- | Home row jamo (keys a s d f g h j k l)
homeRowJamo :: Set Jamo
homeRowJamo = Set.fromList
    [ Consonant 'ㅁ', Consonant 'ㄴ', Consonant 'ㅇ', Consonant 'ㄹ', Consonant 'ㅎ'
    , Vowel 'ㅗ', Vowel 'ㅓ', Vowel 'ㅏ', Vowel 'ㅣ'
    ]

-- | Top row jamo (keys q w e r t y u i o p)
topRowJamo :: Set Jamo
topRowJamo = Set.fromList
    [ Consonant 'ㅂ', Consonant 'ㅈ', Consonant 'ㄷ', Consonant 'ㄱ', Consonant 'ㅅ'
    , Vowel 'ㅛ', Vowel 'ㅕ', Vowel 'ㅑ', Vowel 'ㅐ', Vowel 'ㅔ'
    ]

-- | Bottom row jamo (keys z x c v b n m)
bottomRowJamo :: Set Jamo
bottomRowJamo = Set.fromList
    [ Consonant 'ㅋ', Consonant 'ㅌ', Consonant 'ㅊ', Consonant 'ㅍ'
    , Vowel 'ㅠ', Vowel 'ㅜ', Vowel 'ㅡ'
    ]
