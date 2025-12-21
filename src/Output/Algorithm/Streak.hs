{-# LANGUAGE DeriveGeneric #-}

module Output.Algorithm.Streak
    ( calculateStreak
    , isConsecutiveDay
    ) where

import Data.Time (LocalTime, toGregorian, localDay)
import Data.List (sortBy)
import Data.Ord (comparing)
import Output.Domain.Activity (ActivityEntry(..))

-- | Calculate the current daily streak from activity logs
calculateStreak :: LocalTime -> [ActivityEntry] -> Int
calculateStreak currentDate activities
    | null activities = 0
    | otherwise =
        let sortedActivities = sortBy (comparing (Down . actDate)) activities
            dates = map (extractDate . actDate) sortedActivities
            uniqueDates = dedup dates  -- Keep newest first, don't reverse
        in countConsecutiveDays currentDate uniqueDates

-- | Extract just the date part from a LocalTime
extractDate :: LocalTime -> (Integer, Int, Int)
extractDate localTime =
    let day = localDay localTime
        (year, month, dayOfMonth) = toGregorian day
    in (year, month, dayOfMonth)

-- | Remove duplicate dates
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : dedup (dropWhile (== x) xs)

-- | Count consecutive days from a list of sorted dates
countConsecutiveDays :: LocalTime -> [(Integer, Int, Int)] -> Int
countConsecutiveDays currentDate dates
    | null dates = 0
    | otherwise =
        let (cy, cm, cd) = extractDate currentDate
            expectedDate = (cy, cm, cd)
        in countConsec expectedDate dates 0
  where
    countConsec _ [] count = count
    countConsec expected (d:ds) count
        | d == expected = countConsec (prevDay expected) ds (count + 1)
        | otherwise = count

-- | Get the previous day as a tuple
prevDay :: (Integer, Int, Int) -> (Integer, Int, Int)
prevDay (year, month, day)
    | day > 1 = (year, month, day - 1)
    | month > 1 = (year, month - 1, daysInMonth (month - 1) year)
    | otherwise = (year - 1, 12, 31)
  where
    daysInMonth m y
        | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
        | m `elem` [4, 6, 9, 11] = 30
        | m == 2 && isLeapYear y = 29
        | m == 2 = 28
        | otherwise = 0

-- | Check if it's a leap year
isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- | Check if two dates are consecutive
isConsecutiveDay :: (Integer, Int, Int) -> (Integer, Int, Int) -> Bool
isConsecutiveDay date1 date2 = prevDay date1 == date2

-- | Helper for Down ordering
newtype Down a = Down a deriving (Eq)

instance Ord a => Ord (Down a) where
    compare (Down a) (Down b) = compare b a
