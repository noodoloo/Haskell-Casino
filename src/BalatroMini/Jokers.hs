module BalatroMini.Jokers
  ( jokerEnchantNotes
  , jokerXMult
  , applyJokers
  , extraDiscardCount
  , extraHandCount
  ) where

import Data.List (group, sort)

import BalatroMini.Cards (isFace)
import BalatroMini.Types
import BalatroMini.Utils (showXMult)

jokerEnchantNotes :: [Joker] -> [String]
jokerEnchantNotes activeJokers =
  chipNote ++ multNote
  where
    chipCount = length [joker | joker <- activeJokers, jokerEnchant joker == BonusChips]
    multCount = length [joker | joker <- activeJokers, jokerEnchant joker == LuckyMult]
    chipNote =
      if chipCount > 0
        then ["Enhanced joker +" ++ show (chipCount * 20) ++ " chips"]
        else []
    multNote =
      if multCount > 0
        then ["Enhanced joker +" ++ show (multCount * 2) ++ " mult"]
        else []

jokerXMult :: [Joker] -> [PlayingCard] -> (Float, [String])
jokerXMult activeJokers deck =
  (product factors, notes)
  where
    jokerCount = length activeJokers
    emptySlotFactor = fromIntegral (max 0 (5 - jokerCount)) :: Float
    enhancedCount = length [card | card <- deck, cardEnchant card /= NoEnchant]
    enhancedFactor = 1 + fromIntegral enhancedCount * 0.25
    hasEmptyStage = any ((== EmptyStage) . jokerId) activeJokers
    hasEnhancementEngine = any ((== EnhancementEngine) . jokerId) activeJokers
    factors =
      (if hasEmptyStage then [emptySlotFactor] else [])
        ++ (if hasEnhancementEngine then [enhancedFactor] else [])
    notes =
      (if hasEmptyStage then ["Empty Stage x" ++ showXMult emptySlotFactor ++ " mult"] else [])
        ++ (if hasEnhancementEngine then ["Enhancement Engine x" ++ showXMult enhancedFactor ++ " mult"] else [])

applyJokers :: [Joker] -> [PlayingCard] -> HandType -> (Int, Int, [String])
applyJokers activeJokers selected scoredHandType =
  foldr addJoker (0, 0, []) activeJokers
  where
    faceCount = length (filter (isFace . cardRank) selected)
    aceCount = length (filter ((== Ace) . cardRank) selected)
    redCount = length (filter (\card -> cardSuit card == Hearts || cardSuit card == Diamonds || cardEnchant card == WildSuit) selected)
    groupedRanks = group (sort (map cardRank selected))
    hasPair = any (\sameRanks -> length sameRanks >= 2) groupedRanks
    addJoker joker (chipAcc, multAcc, notesAcc) =
      case jokerId joker of
        FlatMultJoker -> (chipAcc, multAcc + 5, notesAcc ++ ["+5 mult"])
        PairPrinter ->
          if hasPair
            then (chipAcc + 40, multAcc, notesAcc ++ ["Pair Printer +40 chips"])
            else (chipAcc, multAcc, notesAcc)
        FaceCollector ->
          if faceCount > 0
            then (chipAcc, multAcc + faceCount * 2, notesAcc ++ ["Face Collector +" ++ show (faceCount * 2) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        GreedyJoker ->
          if redCount > 0
            then (chipAcc, multAcc + redCount * 2, notesAcc ++ ["Greedy Joker + " ++ show (redCount * 2) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        StraightShooter ->
          if scoredHandType == Straight || scoredHandType == StraightFlush || scoredHandType == RoyalFlush
            then (chipAcc + 80, multAcc, notesAcc ++ ["Straight Shooter +80 chips"])
            else (chipAcc, multAcc, notesAcc)
        BlindPurse -> (chipAcc, multAcc, notesAcc)
        ChipSprinter -> (chipAcc + 25, multAcc, notesAcc ++ ["Chip Sprinter +25 chips"])
        TwinPair ->
          if scoredHandType == TwoPair
            then (chipAcc, multAcc + 6, notesAcc ++ ["Twin Pair +6 mult"])
            else (chipAcc, multAcc, notesAcc)
        AcePilot ->
          if aceCount > 0
            then (chipAcc, multAcc + aceCount * 4, notesAcc ++ ["Ace Pilot +" ++ show (aceCount * 4) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        ExtraDiscard -> (chipAcc, multAcc, notesAcc)
        ExtraHand -> (chipAcc, multAcc, notesAcc)
        EmptyStage -> (chipAcc, multAcc, notesAcc)
        SuitPainter -> (chipAcc, multAcc, notesAcc)
        ShortcutArtist -> (chipAcc, multAcc, notesAcc)
        EnhancementEngine -> (chipAcc, multAcc, notesAcc)

extraDiscardCount :: [Joker] -> Int
extraDiscardCount activeJokers =
  length (filter ((== ExtraDiscard) . jokerId) activeJokers)

extraHandCount :: [Joker] -> Int
extraHandCount activeJokers =
  length (filter ((== ExtraHand) . jokerId) activeJokers)
