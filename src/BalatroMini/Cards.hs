module BalatroMini.Cards
  ( rankLabel
  , suitGlyph
  , enchantShort
  , effectiveSuit
  , effectiveSuitWith
  , suitColorGroup
  , cardChipValue
  , showPlayingCard
  , enchantSuffix
  , drawUpTo
  , refillHandAfterRemoval
  , isStraight
  , rankNumber
  , isFace
  , fillerCard
  , enhancedEnchantments
  , jokerEnhancedEnchantments
  , compareCardsByRank
  , compareCardsBySuit
  ) where

import Data.List (sort)
import Data.Ord (comparing)

import BalatroMini.Types

rankLabel :: Rank -> String
rankLabel rankValue =
  case rankValue of
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

suitGlyph :: Suit -> String
suitGlyph suitValue =
  case suitValue of
    Clubs -> "C"
    Diamonds -> "D"
    Hearts -> "H"
    Spades -> "S"

enchantShort :: Enchant -> String
enchantShort enchantment =
  case enchantment of
    NoEnchant -> ""
    BonusChips -> "+20c"
    LuckyMult -> "+2m"
    TimesMult -> "x1.5"
    GoldSeal -> "$3"
    WildSuit -> "wild"

effectiveSuit :: PlayingCard -> Suit
effectiveSuit card =
  case cardEnchant card of
    WildSuit -> Hearts
    _ -> cardSuit card

effectiveSuitWith :: [Joker] -> PlayingCard -> Suit
effectiveSuitWith activeJokers card =
  let baseSuit = effectiveSuit card
   in if any ((== SuitPainter) . jokerId) activeJokers
        then suitColorGroup baseSuit
        else baseSuit

suitColorGroup :: Suit -> Suit
suitColorGroup suit =
  case suit of
    Hearts -> Hearts
    Diamonds -> Hearts
    Clubs -> Spades
    Spades -> Spades

cardChipValue :: PlayingCard -> Int
cardChipValue card =
  case cardRank card of
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

showPlayingCard :: PlayingCard -> String
showPlayingCard card =
  rankLabel (cardRank card) ++ suitGlyph (cardSuit card) ++ enchantSuffix (cardEnchant card)

enchantSuffix :: Enchant -> String
enchantSuffix enchantment =
  case enchantment of
    NoEnchant -> ""
    BonusChips -> " (+20 chips)"
    LuckyMult -> " (+2 mult)"
    TimesMult -> " (x1.5 mult)"
    GoldSeal -> " (gold)"
    WildSuit -> " (wild)"

drawUpTo :: Int -> [a] -> [a] -> ([a], [a])
drawUpTo target source existing
  | length existing >= target = (existing, source)
drawUpTo _ [] existing = (existing, [])
drawUpTo target (next : rest) existing = drawUpTo target rest (existing ++ [next])

refillHandAfterRemoval :: [PlayingCard] -> [PlayingCard] -> [PlayingCard] -> [PlayingCard] -> Maybe ([PlayingCard], [PlayingCard], [PlayingCard])
refillHandAfterRemoval kept removed currentDraw currentDiscard
  | length currentDraw < cardsNeeded = Nothing
  | otherwise =
      let (drawnCards, finalDraw) = splitAt cardsNeeded currentDraw
       in Just (kept ++ drawnCards, finalDraw, currentDiscard ++ removed)
  where
    cardsNeeded = length removed

isStraight :: [Rank] -> Bool
isStraight ranks =
  let values = sort (map rankNumber ranks)
      wheel = [2, 3, 4, 5, 14]
   in values == wheel || and (zipWith (\a b -> b - a == 1) values (tail values))

rankNumber :: Rank -> Int
rankNumber rankValue =
  case rankValue of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Queen -> 12
    King -> 13
    Ace -> 14

isFace :: Rank -> Bool
isFace rankValue = rankValue == Jack || rankValue == Queen || rankValue == King

fillerCard :: PlayingCard
fillerCard = PlayingCard Ace Spades NoEnchant

enhancedEnchantments :: [Enchant]
enhancedEnchantments = [BonusChips, LuckyMult, TimesMult, GoldSeal, WildSuit]

jokerEnhancedEnchantments :: [Enchant]
jokerEnhancedEnchantments = [BonusChips, LuckyMult, TimesMult, GoldSeal]

compareCardsByRank :: PlayingCard -> PlayingCard -> Ordering
compareCardsByRank cardA cardB =
  comparing cardRank cardA cardB <> comparing cardSuit cardA cardB

compareCardsBySuit :: PlayingCard -> PlayingCard -> Ordering
compareCardsBySuit cardA cardB =
  comparing cardSuit cardA cardB <> comparing cardRank cardA cardB
