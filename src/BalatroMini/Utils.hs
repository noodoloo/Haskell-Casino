module BalatroMini.Utils
  ( showXMult
  , partitionByIndices
  , removeAt
  , safeIndex
  , wrapWords
  , allSame
  , shuffleList
  ) where

import System.Random (RandomGen, randomR)

showXMult :: Float -> String
showXMult factor =
  let rounded = fromIntegral (round (factor * 100)) / 100 :: Float
   in show rounded

partitionByIndices :: [Int] -> [a] -> ([a], [a])
partitionByIndices picked xs =
  foldr split ([], []) (zip [0 ..] xs)
  where
    split (idx, value) (kept, removed) =
      if idx `elem` picked
        then (kept, value : removed)
        else (value : kept, removed)

removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx xs
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

wrapWords :: Int -> String -> [String]
wrapWords maxChars content =
  reverse (foldl addWord [] (words content))
  where
    addWord [] word = [word]
    addWord (current : rest) word
      | length word + 1 + length current <= maxChars = (current ++ " " ++ word) : rest
      | otherwise = word : current : rest

allSame :: Eq a => [a] -> Bool
allSame [] = False
allSame (firstValue : rest) = all (== firstValue) rest

shuffleList :: RandomGen g => [a] -> g -> ([a], g)
shuffleList [] gen = ([], gen)
shuffleList xs gen =
  let (idx, gen1) = randomR (0, length xs - 1) gen
      chosen = xs !! idx
      remainder = take idx xs ++ drop (idx + 1) xs
      (shuffledRest, gen2) = shuffleList remainder gen1
   in (chosen : shuffledRest, gen2)
