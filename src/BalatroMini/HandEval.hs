module BalatroMini.HandEval
  ( evaluateHand
  , evaluateBestHand
  , maximumByHand
  , compareHandResult
  , handRankValue
  , subsetStrength
  , nonEmptySubsets
  ) where

import Data.List (group, sort, sortOn)

import BalatroMini.Cards (cardChipValue, effectiveSuitWith, isStraight)
import BalatroMini.Types
import BalatroMini.Utils (allSame)

evaluateHand :: [Joker] -> [PlayingCard] -> HandScore
evaluateHand activeJokers cards =
  let ranks = sort (map cardRank cards)
      suits = map (effectiveSuitWith activeJokers) cards
      grouped = sortOn (negate . length) (group ranks)
      counts = sortOn negate (map length grouped)
      hasShortcut = any ((== ShortcutArtist) . jokerId) activeJokers
      isFlushHand = (length cards == 5 || hasShortcut && length cards == 4) && allSame suits
      isStraightHand = (length cards == 5 || hasShortcut && length cards == 4) && isStraight ranks
      isRoyalHand = ranks == [Ten, Jack, Queen, King, Ace]
   in case () of
        _
          | counts == [5] && isFlushHand -> HandScore FlushFive 180 16
          | counts == [3, 2] && isFlushHand -> HandScore FlushHouse 160 14
          | isRoyalHand && isFlushHand -> HandScore RoyalFlush 120 10
          | isStraightHand && isFlushHand -> HandScore StraightFlush 100 8
          | counts == [5] -> HandScore FiveKind 140 12
          | counts == [4] || counts == [4, 1] -> HandScore FourKind 60 7
          | counts == [3, 2] -> HandScore FullHouse 45 4
          | isFlushHand -> HandScore Flush 35 4
          | isStraightHand -> HandScore Straight 40 4
          | counts == [3] || counts == [3, 1] || counts == [3, 1, 1] -> HandScore ThreeKind 35 3
          | counts == [2, 2] || counts == [2, 2, 1] -> HandScore TwoPair 25 2
          | counts == [2] || counts == [2, 1] || counts == [2, 1, 1] || counts == [2, 1, 1, 1] -> HandScore OnePair 15 2
          | otherwise -> HandScore HighCard 5 1

evaluateBestHand :: [Joker] -> [PlayingCard] -> (HandScore, [PlayingCard])
evaluateBestHand _ [] = (HandScore HighCard 5 1, [])
evaluateBestHand activeJokers cards =
  maximumByHand
    [ (evaluateHand activeJokers subset, subset)
    | subset <- nonEmptySubsets cards
    , length subset <= 5
    ]

maximumByHand :: [(HandScore, [PlayingCard])] -> (HandScore, [PlayingCard])
maximumByHand [] = (HandScore HighCard 5 1, [])
maximumByHand (firstResult : restResults) =
  foldl chooseBetter firstResult restResults
  where
    chooseBetter best current =
      if compareHandResult current best == GT
        then current
        else best

compareHandResult :: (HandScore, [PlayingCard]) -> (HandScore, [PlayingCard]) -> Ordering
compareHandResult (scoreA, cardsA) (scoreB, cardsB) =
  compare (handRankValue (handType scoreA), negate (length cardsA), subsetStrength cardsA)
          (handRankValue (handType scoreB), negate (length cardsB), subsetStrength cardsB)

handRankValue :: HandType -> Int
handRankValue handKind =
  case handKind of
    HighCard -> 0
    OnePair -> 1
    TwoPair -> 2
    ThreeKind -> 3
    Straight -> 4
    Flush -> 5
    FullHouse -> 6
    FourKind -> 7
    StraightFlush -> 8
    RoyalFlush -> 9
    FiveKind -> 10
    FlushHouse -> 11
    FlushFive -> 12

subsetStrength :: [PlayingCard] -> Int
subsetStrength = sum . map cardChipValue

nonEmptySubsets :: [a] -> [[a]]
nonEmptySubsets [] = []
nonEmptySubsets (x : xs) =
  [x] : map (x :) rest ++ rest
  where
    rest = nonEmptySubsets xs
