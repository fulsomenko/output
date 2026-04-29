module Main where

import Test.Hspec
import qualified Output.Algorithm.SpacedRepetitionSpec as SR
import qualified Output.Algorithm.StreakSpec as Streak
import qualified Output.Algorithm.SRSSpec as SRS

main :: IO ()
main = hspec $ do
    describe "SpacedRepetition" SR.spec
    describe "Streak" Streak.spec
    describe "SRS" SRS.spec
