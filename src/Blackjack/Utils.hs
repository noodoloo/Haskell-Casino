module Blackjack.Utils
  ( formatMoney
  , trimMessage
  , replaceAt
  ) where

import Numeric (showFFloat)

formatMoney :: Double -> String
formatMoney amount =
  if abs (amount - fromIntegral (round amount :: Int)) < 0.001
    then show (round amount :: Int)
    else showFFloat (Just 1) amount ""

trimMessage :: Int -> String -> String
trimMessage maxLen msg
  | length msg <= maxLen = msg
  | maxLen <= 3 = take maxLen msg
  | otherwise = take (maxLen - 3) msg ++ "..."

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt idx replacement xs = take idx xs ++ replacement ++ drop (idx + 1) xs
