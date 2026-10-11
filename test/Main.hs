module Main where

import Test.Hspec
import qualified Output.Algorithm.SpacedRepetitionSpec as SR
import qualified Output.Algorithm.StreakSpec as Streak
import qualified Output.Algorithm.SRSSpec as SRS
import qualified Output.Dict.KrdictSpec as Krdict
import qualified Output.Dict.SuggestSpec as Suggest
import qualified Output.TUI.LookupSpec as Lookup

main :: IO ()
main = hspec $ do
    describe "SpacedRepetition" SR.spec
    describe "Streak" Streak.spec
    describe "SRS" SRS.spec
    describe "Krdict" Krdict.spec
    describe "Suggest" Suggest.spec
    describe "Lookup" Lookup.spec
