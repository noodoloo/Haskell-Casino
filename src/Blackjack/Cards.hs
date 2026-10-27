module Blackjack.Cards
  ( hasNaturalBlackjack
  , hasFiveCardCharlie
  , handTotal
  , isSoft
  , adjustForAces
  , cardHighValue
  , cardLowValue
  , fullDeck
  , shuffleDeck
  , drawCard
  , showCard
  , showRankLong
  , showRankShort
  , showSuitLong
  , showSuitShort
  ) where

import System.Random (RandomGen, randomR)

import Blackjack.Types

hasNaturalBlackjack :: PlayerHand -> Bool
hasNaturalBlackjack hand =
  blackjackEligible hand && length (cards hand) == 2 && handTotal (cards hand) == 21

hasFiveCardCharlie :: PlayerHand -> Bool
hasFiveCardCharlie hand =
  length (cards hand) >= 5 && handTotal (cards hand) <= 21

handTotal :: [Card] -> Int
handTotal hand = adjustForAces rawTotal aceCount
  where
    values = map cardHighValue hand
    rawTotal = sum values
    aceCount = length (filter ((== Ace) . rank) hand)

isSoft :: [Card] -> Bool
isSoft hand =
  aceCount > 0 && bestTotal <= 21 && bestTotal /= hardTotal
  where
    bestTotal = handTotal hand
    hardTotal = sum (map cardLowValue hand)
    aceCount = length (filter ((== Ace) . rank) hand)

adjustForAces :: Int -> Int -> Int
adjustForAces total aceCount
  | total <= 21 = total
  | aceCount == 0 = total
  | otherwise = adjustForAces (total - 10) (aceCount - 1)

cardHighValue :: Card -> Int
cardHighValue card =
  case rank card of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11

cardLowValue :: Card -> Int
cardLowValue card =
  case rank card of
    Ace -> 1
    _ -> cardHighValue card

fullDeck :: [Card]
fullDeck = [Card r s | s <- [minBound .. maxBound], r <- [minBound .. maxBound]]

shuffleDeck :: RandomGen g => [Card] -> g -> ([Card], g)
shuffleDeck [] gen = ([], gen)
shuffleDeck xs gen =
  let (idx, nextGen) = randomR (0, length xs - 1) gen
      chosen = xs !! idx
      remainder = take idx xs ++ drop (idx + 1) xs
      (rest, finalGen) = shuffleDeck remainder nextGen
   in (chosen : rest, finalGen)

drawCard :: [Card] -> (Card, [Card])
drawCard [] = error "Deck unexpectedly empty."
drawCard (card : rest) = (card, rest)

showCard :: Card -> String
showCard (Card r s) = showRankLong r ++ " of " ++ showSuitLong s

showRankLong :: Rank -> String
showRankLong r =
  case r of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"
    Ace -> "Ace"

showRankShort :: Rank -> String
showRankShort r =
  case r of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

showSuitLong :: Suit -> String
showSuitLong s =
  case s of
    Clubs -> "Clubs"
    Diamonds -> "Diamonds"
    Hearts -> "Hearts"
    Spades -> "Spades"

showSuitShort :: Suit -> String
showSuitShort s =
  case s of
    Clubs -> "C"
    Diamonds -> "D"
    Hearts -> "H"
    Spades -> "S"
