{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.App
    ( runTUI
    , app
    , theAttrMap
    ) where

import Brick
import Brick.Widgets.Border.Style (unicodeBold)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCross

import Output.TUI.Types
import Output.TUI.Draw (drawUI)
import Output.TUI.Events (handleEvent)
import Output.Repository.Json (runJsonRepository)
import Output.Repository.Class (getAllVocabCards, getProgress)

-- | Brick application definition
app :: App AppState AppEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const theAttrMap
    }

-- | Attribute map for styling
theAttrMap :: AttrMap
theAttrMap = attrMap V.defAttr
    [ (titleAttr, fg V.cyan `V.withStyle` V.bold)
    , (menuAttr, V.defAttr)
    , (menuSelectedAttr, fg V.cyan `V.withStyle` V.bold)
    , (promptAttr, fg V.yellow)
    , (inputAttr, V.defAttr)
    , (correctAttr, fg V.green `V.withStyle` V.bold)
    , (incorrectAttr, fg V.red)
    , (hintAttr, fg V.blue)
    , (statsAttr, fg V.white)
    ]

-- | Run the TUI application
runTUI :: IO ()
runTUI = do
    -- Load vocabulary and progress
    cards <- runJsonRepository getAllVocabCards
    progress <- runJsonRepository getProgress

    let initialState = initialAppState
            { asVocabCards = cards
            , asProgress = Just progress
            }

    -- Build vty and run
    let buildVty = VCross.mkVty V.defaultConfig
    initialVty <- buildVty
    _finalState <- customMain initialVty buildVty Nothing app initialState

    -- Show goodbye message
    putStrLn "Thanks for practicing! 감사합니다!"
