{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration with the KRDICT Open API (krdict.korean.go.kr).
--
-- KRDICT returns an XML document. The project has no XML parsing library in its
-- dependency set, so this module ships a small, self-contained parser that
-- understands the exact shape KRDICT emits:
--
--   <channel>
--     <item>
--       <word>나무</word>
--       <pos>명사</pos>
--       <sense>
--         <definition>...</definition>
--         <translation>
--           <trans_lang>영어</trans_lang>
--           <trans_word>tree</trans_word>
--           <trans_dfn>A plant with a hard stem...</trans_dfn>
--         </translation>
--       </sense>
--     </item>
--   </channel>
--
-- Every field is a child element with text content, and each <sense> nests
-- several <translation> elements (one per target language); the English one is
-- the one tagged trans_lang = 영어.
module Output.Dict.Krdict
    ( KrdictWord(..)
    , KrdictSense(..)
    , KrdictTranslation(..)
    , KrdictError(..)
    , krdictLookup
    , krdictParse
    , percentEncode
    , krdictWordClass
    , krdictToCard
    , englishTranslation
    , englishTranslations
    , saveKrdictWord
    ) where

import Control.Exception (try, SomeException)
import Data.Aeson (encode)
import Data.Word (Word8)
import Data.ByteString (unpack)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Network.HTTP.Client
import Network.HTTP.Types (statusCode, statusIsSuccessful)
import System.Directory (createDirectoryIfMissing)

import Output.Domain.Types
    ( TOPIK_Level
    , VocabularyCard(..)
    , VocabularyId(..)
    , WordClass(..)
    )
import Output.Repository.Json (loadLearnedWords)

-- | One word entry from a KRDICT search result (an <item>).
data KrdictWord = KrdictWord
    { kwTargetCode    :: Int    -- ^ KRDICT's stable identifier for the word
    , kwWord          :: Text   -- ^ the headword (e.g. 나무)
    , kwPronunciation :: Text   -- ^ "" when KRDICT does not supply one
    , kwWordGrade     :: Text   -- ^ e.g. 초급, or "" when absent
    , kwPos           :: Text   -- ^ part of speech, e.g. 명사
    , kwLink          :: Text   -- ^ canonical KRDICT entry URL
    , kwSenses        :: [KrdictSense]
    } deriving (Show, Eq)

-- | One sense (definition) within a word (a <sense>).
data KrdictSense = KrdictSense
    { ksOrder        :: Int
    , ksDefinition   :: Text
    , ksTranslations :: [KrdictTranslation]
    } deriving (Show, Eq)

-- | One translation of a sense into a specific language (a <translation>).
data KrdictTranslation = KrdictTranslation
    { ktLang :: Text   -- ^ e.g. 영어, 일본어
    , ktWord :: Text   -- ^ the short translation (e.g. tree)
    , ktDfn  :: Text   -- ^ the translated full definition
    } deriving (Show, Eq)

-- | Kinds of failure a KRDICT lookup can hit.
data KrdictError
    = KrdictHttpError Text    -- ^ transport / connection failure
    | KrdictApiError  Text    -- ^ non-2xx status, or empty result
    | KrdictParseError Text   -- ^ the response body was not valid KRDICT XML
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Percent-encode a value so it is safe to place in a URL query string.
-- Encodes every UTF-8 byte except the unreserved set [A-Za-z0-9-_.~].
percentEncode :: Text -> String
percentEncode t = concatMap encodeByte (unpack (TEnc.encodeUtf8 t))
  where
    safe :: String
    safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"

    toChar :: Word8 -> Char
    toChar b = toEnum (fromIntegral b)

    hexNib :: Int -> Char
    hexNib n
        | n <= 9  = toEnum (fromIntegral (n + 48))   -- '0'..'9'
        | n <= 15 = toEnum (fromIntegral (n + 87))   -- 'a'..'f'
        | otherwise = 'z'

    encodeByte :: Word8 -> String
    encodeByte b
        | let c = toChar b
        , c `elem` safe = [c]
        | otherwise =
            let hi = hexNib (fromIntegral (b `div` 16) :: Int)
                lo = hexNib (fromIntegral (b `mod` 16) :: Int)
            in ['%', hi, lo]

-- | Search KRDICT for a word. Pure body parsing is split out as 'krdictParse'
-- so it can be unit-tested without any network access.
krdictLookup :: Text -- ^ API key (the key= query parameter)
             -> Text  -- ^ the word to look up (the q= query parameter)
             -> IO (Either KrdictError [KrdictWord])
krdictLookup apiKey word = do
    result <- try go
    case result of
        Left (ex :: SomeException) ->
            pure $ Left (KrdictHttpError (T.pack (show ex)))
        Right v -> pure v
  where
    go :: IO (Either KrdictError [KrdictWord])
    go = do
        manager <- newManager defaultManagerSettings
        let url = "https://krdict.korean.go.kr/api/search"
                <> "?key=" <> percentEncode apiKey
                <> "&q=" <> percentEncode word
                <> "&part=word"
                <> "&num=10"
        req <- parseRequest url
        let req' = req
                { method          = "GET"
                , responseTimeout = responseTimeoutNone
                }
        resp <- httpLbs req' manager
        let code = statusCode (responseStatus resp)
        if not (statusIsSuccessful (responseStatus resp))
            then pure $ Left (KrdictApiError ("KRDICT returned HTTP " <> T.pack (show code)))
            else
                let text = TEnc.decodeUtf8 (BL.toStrict (responseBody resp))
                in case krdictParse text of
                    Left e  -> pure $ Left (KrdictParseError ("could not parse KRDICT response: " <> e))
                    Right ws ->
                        if null ws
                            then pure $ Left (KrdictApiError ("no KRDICT results for " <> word))
                            else pure (Right ws)

-- | Parse a KRDICT XML response body into its list of words. Pure.
krdictParse :: Text -> Either Text [KrdictWord]
krdictParse body = case parseXml body of
    Left e   -> Left e
    Right ws -> pure ws

-- | The English translation for a sense, if one is present.
englishTranslation :: KrdictSense -> Maybe KrdictTranslation
englishTranslation sense =
    listToMaybe (filter (\t -> ktLang t == "영어") (ksTranslations sense))

-- | Every English short-translation across a word's senses (one per sense).
englishTranslations :: KrdictWord -> [Text]
englishTranslations w =
    mapMaybe (\s -> ktWord <$> englishTranslation s) (kwSenses w)

-- | Map a KRDICT part-of-speech string (Korean) to a WordClass, best effort.
-- KRDICT returns terms such as 명사, 동사, 형용사; we match on their key
-- substrings, most specific first.
krdictWordClass :: Text -> WordClass
krdictWordClass t
    | "명사"   `T.isInfixOf` t = Noun           -- noun (also 의존명사, dependent noun)
    | "동사"   `T.isInfixOf` t = Verb           -- verb
    | "형용사" `T.isInfixOf` t = Adjective      -- descriptive verb
    | "부사"   `T.isInfixOf` t = Adverb         -- adverb
    | "조사"   `T.isInfixOf` t = Particle       -- particle
    | "의존"   `T.isInfixOf` t = BoundMorpheme  -- dependent morpheme (의존사/의존형태소)
    | "수사"   `T.isInfixOf` t = Counter        -- numeral / counting word
    | "감탄사" `T.isInfixOf` t = Interjection   -- interjection
    | "관형사" `T.isInfixOf` t = Determiner     -- determiner
    | otherwise                   = Unknown

-- | Build a VocabularyCard from a KRDICT word, treating the KRDICT target_code
-- as the card's identity. Used for the display path; use 'saveKrdictWord'
-- when you want the app's own ID assignment and de-duplication.
krdictToCard :: TOPIK_Level -> KrdictWord -> VocabularyCard
krdictToCard level w = buildCard (VocabularyId (kwTargetCode w)) level w

buildCard :: VocabularyId -> TOPIK_Level -> KrdictWord -> VocabularyCard
buildCard vid level w = VocabularyCard
    { vocabId             = vid
    , wordClass           = krdictWordClass (kwPos w)
    , korean              = kwWord w
    , romanization        = ""
    , english             = englishTranslations w
    , topicLevel          = level
    , exampleSentences    = []
    , exampleTranslations = []
    }

-- | Merge a KRDICT word into the learned-word store, following the same
-- de-duplication, ID assignment and persistence pattern as
-- 'Output.Repository.Json.saveLearnedWord'.
--
-- Returns 'Nothing' if the word (matched on its Korean text) is already
-- present, or 'Just' the freshly saved card on success.
saveKrdictWord :: TOPIK_Level -> KrdictWord -> IO (Maybe VocabularyCard)
saveKrdictWord level w = do
    existing <- loadLearnedWords
    let word    = kwWord w
        already = word `elem` map korean existing
    if already
        then pure Nothing
        else do
            let existingIds = map (\c -> let VocabularyId n = vocabId c in n) existing
                maxId    = foldr max 100000 existingIds
                nextId   = VocabularyId (maxId + 1)
                newCard  = buildCard nextId level w
            createDirectoryIfMissing True "data/user-data"
            BL.writeFile "data/user-data/learned-words.json" (encode (existing ++ [newCard]))
            pure (Just newCard)

-- ---------------------------------------------------------------------------
-- XML parsing (hand-rolled; no external dependencies)
-- ---------------------------------------------------------------------------

-- | A node in the small XML tree the parser builds.
data XmlElem
    = XmlElem
        { xeName   :: Text      -- ^ element name (empty for a pseudo-node)
        , xeText   :: Text      -- ^ direct text content, stripped
        , xeElems  :: [XmlElem] -- ^ child elements, in document order
        } deriving (Show, Eq)

-- Parse the whole document down to the single root element and extract words.
parseXml :: Text -> Either Text [KrdictWord]
parseXml input = parseTop (T.dropWhile (\c -> c /= '<') input)
  where
    parseTop :: Text -> Either Text [KrdictWord]
    parseTop s
        | T.isPrefixOf "<?" s =
            let (_, after) = T.breakOn "?>" s
            in parseTop (T.dropWhile (\c -> c /= '<') (T.drop 2 after))
        | T.isPrefixOf "<!--" s =
            let (_, after) = T.breakOn "-->" s
            in parseTop (T.dropWhile (\c -> c /= '<') (T.drop 3 after))
        | T.isPrefixOf "</" s =
            Left "unexpected closing tag at top level"
        | T.isPrefixOf "<" s =
            let (root, _rest) = parseElement s
            in pure (wordsFromXml [root])
        | otherwise = Left "malformed KRDICT XML: no root element found"

-- Parse one element (which may carry attributes, text and/or children) starting
-- from its opening tag. Returns the built node and the remaining input.
parseElement :: Text -> (XmlElem, Text)
parseElement s0
    | not (T.isPrefixOf "<" s0) = (XmlElem T.empty T.empty [], s0)
    | otherwise =
        let s1      = T.drop 1 s0                     -- drop leading '<'
            (name, afterName) = breakNamePart s1
        in
            case skipTagAfterName afterName of
                (True, rest)  -> (XmlElem name T.empty [], rest)
                (False, rest) -> closeElement name rest

-- Read the element name (letters/digits and :_-.) up to the first non-name char.
breakNamePart :: Text -> (Text, Text)
breakNamePart s = (T.takeWhile isNameChar s, T.dropWhile isNameChar s)

-- Skip any attribute text up to the closing '>', reporting whether the tag
-- self-closed (ends with "/"). Returns (selfClosing, inputAfterClose).
skipTagAfterName :: Text -> (Bool, Text)
skipTagAfterName s
    | T.null s = (False, s)
    | otherwise =
        let (before, after) = T.breakOn ">" s
            selfClose       = not (T.null (T.stripEnd before))
                                && T.isSuffixOf "/" (T.stripEnd before)
        in (selfClose, T.drop 1 after)

-- Parse the children of an element until its matching close tag. The close tag
-- is consumed; the returned node accumulates stripped text plus child elements.
closeElement :: Text -> Text -> (XmlElem, Text)
closeElement tag input = run [] (T.empty) input
  where
    run acc accText rest
        | T.null (T.strip rest) = (XmlElem tag (T.strip accText) (reverse acc), rest)
        | T.isPrefixOf "</" rest =
            let (closeName, restAfter) = readClosingTag rest
            in if closeName == tag
                   then (XmlElem tag (T.strip accText) (reverse acc), restAfter)
                   else (XmlElem tag (T.strip accText) (reverse acc), rest)
        | T.isPrefixOf "<!--" rest =
            let (_, after) = T.breakOn "-->" rest
            in run acc accText (T.drop 3 after)
        | T.isPrefixOf "<?" rest =
            let (_, after) = T.breakOn "?>" rest
            in run acc accText (T.drop 2 after)
        | T.isPrefixOf "<" rest =
            let (el, rest') = parseElement rest
            in run (el : acc) accText rest'
        | otherwise =
            let (chunk, rest') = T.breakOn "<" rest
            in run acc (accText <> chunk) rest'

-- A close tag is '</' + name + '>'. Returns (name, inputAfterClose).
readClosingTag :: Text -> (Text, Text)
readClosingTag s0 =
    let s1     = T.drop 2 s0                -- drop '</'
        (name, s2) = T.breakOn ">" s1
    in (name, T.drop 1 s2)

isNameChar :: Char -> Bool
isNameChar c = isAlpha c || isDigit c || c `elem` (":-._" :: String)

-- Extract the list of words from a parsed KRDICT doc. KRDICT nests its items
-- under wrapper elements (<response><document><channel>), so collect <item>
-- nodes at any depth rather than only direct children of the root.
wordsFromXml :: [XmlElem] -> [KrdictWord]
wordsFromXml roots = concatMap (map itemToWord . itemsIn) roots
  where
    itemsIn :: XmlElem -> [XmlElem]
    itemsIn e = direct ++ concatMap itemsIn (descend)
      where
        direct  = childrenNamed "item" e
        descend = filter (\c -> xeName c /= "item") (xeElems e)

-- First (in document order) child with the given name.
childText :: Text -> XmlElem -> Text
childText name node = textOf (fromMaybe emptyElem (listToMaybe (childrenNamed name node)))
  where
    emptyElem = XmlElem T.empty T.empty []

childrenNamed :: Text -> XmlElem -> [XmlElem]
childrenNamed name node = filter (\c -> xeName c == name) (xeElems node)

-- | Stripped direct text content of an element (leaf text in KRDICT's output).
textOf :: XmlElem -> Text
textOf = T.strip . xeText

-- Convert one <item> element into a KrdictWord.
itemToWord :: XmlElem -> KrdictWord
itemToWord item = KrdictWord
    { kwTargetCode    = fromMaybe 0 (readInt (childText "target_code" item))
    , kwWord          = childText "word" item
    , kwPronunciation = childText "pronunciation" item
    , kwWordGrade     = childText "word_grade" item
    , kwPos           = childText "pos" item
    , kwLink          = childText "link" item
    , kwSenses        = map senseToSense (childrenNamed "sense" item)
    }

senseToSense :: XmlElem -> KrdictSense
senseToSense sense = KrdictSense
    { ksOrder        = fromMaybe 0 (readInt (childText "sense_order" sense))
    , ksDefinition   = childText "definition" sense
    , ksTranslations = map transToTrans (childrenNamed "translation" sense)
    }

transToTrans :: XmlElem -> KrdictTranslation
transToTrans t = KrdictTranslation
    { ktLang = childText "trans_lang" t
    , ktWord = childText "trans_word" t
    , ktDfn  = childText "trans_dfn" t
    }

-- | Parse a Text as an Int when possible (KRDICT fields like target_code).
readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
    (n, "") : _ -> Just n
    _           -> Nothing
