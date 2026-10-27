module BalatroMini.Scoring
  ( scoreCards
  , scoreValue
  , scoreMoney
  , describeScoredHand
  , scoreTotalMult
  , bonusSummary
  , moneyNotes
  , xMultNotes
  , xMultSummary
  , handUpgradeLevel
  , handUpgradeLevelFrom
  , upgradeHand
  , applyHandUpgrade
  ) where

import Data.List (intercalate)

import BalatroMini.Cards (cardChipValue)
import BalatroMini.Content (handTypeName, upgradeChipsPerLevel, upgradeMultPerLevel)
import BalatroMini.HandEval (evaluateBestHand)
import BalatroMini.Jokers (applyJokers, jokerEnchantNotes, jokerXMult)
import BalatroMini.Types
import BalatroMini.Utils (showXMult)

scoreCards :: [Joker] -> [(HandType, Int)] -> [PlayingCard] -> [PlayingCard] -> ScoreBreakdown
scoreCards activeJokers upgrades deck selected =
  let (base, scoringCards) = evaluateBestHand activeJokers selected
      upgradedBase = applyHandUpgrade upgrades base
      rankChips = sum (map cardChipValue scoringCards)
      enchantChips = sum [20 | card <- scoringCards, cardEnchant card == BonusChips]
      enchantMult = sum [2 | card <- scoringCards, cardEnchant card == LuckyMult]
      timesMultCount = length [card | card <- scoringCards, cardEnchant card == TimesMult]
      jokerEnchantChips = sum [20 | joker <- activeJokers, jokerEnchant joker == BonusChips]
      jokerEnchantMult = sum [2 | joker <- activeJokers, jokerEnchant joker == LuckyMult]
      jokerTimesMultCount = length [joker | joker <- activeJokers, jokerEnchant joker == TimesMult]
      enchantMultFactor = 1.5 ^^ (timesMultCount + jokerTimesMultCount)
      (jokerXMultFactor, jokerXMultNotes) = jokerXMult activeJokers deck
      goldMoney = sum [3 | card <- scoringCards, cardEnchant card == GoldSeal]
      (jokerChipBonus, jokerMultBonus, jokerNotes) = applyJokers activeJokers scoringCards (handType upgradedBase)
   in ScoreBreakdown
        { scoreBase = upgradedBase
        , scoreCardsUsed = scoringCards
        , scoreChipBonus = rankChips + enchantChips + jokerEnchantChips + jokerChipBonus
        , scoreMultBonus = enchantMult + jokerEnchantMult + jokerMultBonus
        , scoreMultFactor = enchantMultFactor * jokerXMultFactor
        , scoreMoneyGain = goldMoney
        , scoreNotes =
            jokerNotes
              ++ jokerEnchantNotes activeJokers
              ++ xMultNotes (timesMultCount + jokerTimesMultCount) enchantMultFactor
              ++ jokerXMultNotes
              ++ moneyNotes goldMoney
        }

scoreValue :: ScoreBreakdown -> Int
scoreValue breakdown =
  (baseChips (scoreBase breakdown) + scoreChipBonus breakdown)
    * scoreTotalMult breakdown

scoreMoney :: ScoreBreakdown -> Int
scoreMoney = scoreMoneyGain

describeScoredHand :: ScoreBreakdown -> String
describeScoredHand breakdown =
  handTypeName (handType (scoreBase breakdown))
    ++ " | "
    ++ show (baseChips (scoreBase breakdown) + scoreChipBonus breakdown)
    ++ " x "
    ++ show (scoreTotalMult breakdown)
    ++ bonusSummary (scoreNotes breakdown)

scoreTotalMult :: ScoreBreakdown -> Int
scoreTotalMult breakdown =
  floor $
    fromIntegral (baseMult (scoreBase breakdown) + scoreMultBonus breakdown)
      * scoreMultFactor breakdown

bonusSummary :: [String] -> String
bonusSummary [] = ""
bonusSummary notes = " | " ++ intercalate ", " notes

moneyNotes :: Int -> [String]
moneyNotes 0 = []
moneyNotes value = ["Gold card paid $" ++ show value]

xMultNotes :: Int -> Float -> [String]
xMultNotes 0 _ = []
xMultNotes count factor =
  ["X mult " ++ showXMult factor ++ " from " ++ show count ++ " source(s)"]

xMultSummary :: Float -> String
xMultSummary factor
  | factor == 1 = ""
  | otherwise = " x" ++ showXMult factor

handUpgradeLevel :: HandType -> Game -> Int
handUpgradeLevel handKind game =
  handUpgradeLevelFrom handKind (handUpgrades game)

handUpgradeLevelFrom :: HandType -> [(HandType, Int)] -> Int
handUpgradeLevelFrom handKind upgrades =
  case lookup handKind upgrades of
    Just level -> level
    Nothing -> 0

upgradeHand :: HandType -> [(HandType, Int)] -> [(HandType, Int)]
upgradeHand handKind [] = [(handKind, 1)]
upgradeHand handKind ((currentKind, level) : rest)
  | handKind == currentKind = (currentKind, level + 1) : rest
  | otherwise = (currentKind, level) : upgradeHand handKind rest

applyHandUpgrade :: [(HandType, Int)] -> HandScore -> HandScore
applyHandUpgrade upgrades score =
  let level = handUpgradeLevelFrom (handType score) upgrades
   in score
        { baseChips = baseChips score + level * upgradeChipsPerLevel
        , baseMult = baseMult score + level * upgradeMultPerLevel
        }
