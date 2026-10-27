module BalatroMini.Economy
  ( pointsSuffix
  , blindPurseIncome
  , jokerGoldBlindIncome
  , jokerGoldSummary
  , interestIncome
  , interestSummary
  , jokerSellValue
  ) where

import BalatroMini.Types

pointsSuffix :: Int -> String
pointsSuffix 0 = "."
pointsSuffix cash = " and earned $" ++ show cash ++ "."

blindPurseIncome :: [Joker] -> Int
blindPurseIncome activeJokers =
  3 * length (filter ((== BlindPurse) . jokerId) activeJokers)

jokerGoldBlindIncome :: [Joker] -> Int
jokerGoldBlindIncome activeJokers =
  3 * length (filter ((== GoldSeal) . jokerEnchant) activeJokers)

jokerGoldSummary :: Int -> String
jokerGoldSummary 0 = ""
jokerGoldSummary goldCash = " including $" ++ show goldCash ++ " joker gold"

interestIncome :: Int -> Int
interestIncome bankroll =
  min bankroll 25 `div` 5

interestSummary :: Int -> String
interestSummary 0 = ""
interestSummary interestCash = " including $" ++ show interestCash ++ " interest"

jokerSellValue :: Joker -> Int
jokerSellValue joker =
  max 1 ((jokerCost joker * 3 + 5) `div` 10)
