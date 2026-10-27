module BalatroMini.Round
  ( toggleExpandedJoker
  , requestSellExpandedJoker
  , sellJoker
  , toggleIndex
  , discardSelected
  , playSelectedHand
  , advanceBlind
  , sortHandBy
  ) where

import Data.List (sort, sortBy)

import BalatroMini.Cards (refillHandAfterRemoval)
import BalatroMini.Content (blinds)
import BalatroMini.Economy
  ( blindPurseIncome
  , interestIncome
  , interestSummary
  , jokerGoldBlindIncome
  , jokerGoldSummary
  , jokerSellValue
  , pointsSuffix
  )
import BalatroMini.Scoring (describeScoredHand, scoreCards, scoreMoney, scoreValue)
import BalatroMini.Shop (openShop)
import BalatroMini.Types
import BalatroMini.Utils (partitionByIndices, removeAt, safeIndex)

toggleExpandedJoker :: Int -> Game -> Game
toggleExpandedJoker idx game
  | expandedJokerIndex game == Just idx = game {expandedJokerIndex = Nothing, confirmSellJokerIndex = Nothing}
  | otherwise = game {expandedJokerIndex = Just idx, confirmSellJokerIndex = Nothing}

requestSellExpandedJoker :: Game -> Game
requestSellExpandedJoker game =
  case expandedJokerIndex game >>= (`safeIndex` jokers game) of
    Just joker ->
      game
        { confirmSellJokerIndex = expandedJokerIndex game
        , message = "Sell " ++ jokerName joker ++ " for $" ++ show (jokerSellValue joker) ++ "? Enter confirms, C cancels."
        }
    Nothing -> game

sellJoker :: Int -> Game -> Game
sellJoker idx game =
  case safeIndex idx (jokers game) of
    Just joker ->
      game
        { money = money game + jokerSellValue joker
        , jokers = removeAt idx (jokers game)
        , expandedJokerIndex = Nothing
        , confirmSellJokerIndex = Nothing
        , message = "Sold " ++ jokerName joker ++ " for $" ++ show (jokerSellValue joker) ++ "."
        }
    Nothing -> game {confirmSellJokerIndex = Nothing}

toggleIndex :: Int -> Game -> Game
toggleIndex idx game
  | idx >= length (handCards game) = game
  | idx `elem` selectedIndices game =
      game {selectedIndices = filter (/= idx) (selectedIndices game)}
  | length (selectedIndices game) >= 5 =
      game {message = "You can only select up to 5 cards."}
  | otherwise =
      game {selectedIndices = sort (idx : selectedIndices game)}

discardSelected :: Game -> Game
discardSelected game
  | discardsLeft game <= 0 = game {message = "No discards left on this blind."}
  | null (selectedIndices game) = game {message = "Select cards to discard first."}
  | otherwise =
      let (kept, tossed) = partitionByIndices (selectedIndices game) (handCards game)
       in case refillHandAfterRemoval kept tossed (drawPile game) (discardPile game) of
            Just (refilled, newDrawPile, newDiscard) ->
              game
                { handCards = refilled
                , drawPile = newDrawPile
                , discardPile = newDiscard
                , selectedIndices = []
                , discardsLeft = discardsLeft game - 1
                , playedPreview = []
                , scoredPreview = []
                , previewTimer = 0
                , lastHandScore = 0
                , hoveredCardIndex = Nothing
                , lastPlayedLabel = "Discarded " ++ show (length tossed) ++ " card(s)"
                , message = "Discard used. Build a stronger hand."
                }
            Nothing ->
              game
                { handCards = kept
                , drawPile = []
                , discardPile = discardPile game ++ tossed
                , selectedIndices = []
                , playedPreview = []
                , scoredPreview = []
                , previewTimer = 0
                , hoveredCardIndex = Nothing
                , phase = RunLost
                , lastPlayedLabel = "Deck exhausted"
                , message = "You ran out of cards to draw. Press R for a fresh run."
                }

playSelectedHand :: Game -> Game
playSelectedHand game
  | null (selectedIndices game) = game {message = "Select at least one card to play."}
  | handsLeft game <= 0 = game {message = "No hands left on this blind."}
  | otherwise =
      let selectedCards = map (handCards game !!) (selectedIndices game)
          score = scoreCards (jokers game) (handUpgrades game) (permanentDeck game) selectedCards
          handTotalScore = scoreValue score
          moneyGain = scoreMoney score
          newScore = scoreThisBlind game + handTotalScore
          blind = blinds !! currentBlindIndex game
          wonBlind = newScore >= blindTarget blind
          blindBonusCash = blindPurseIncome (jokers game)
          jokerGoldCash = jokerGoldBlindIncome (jokers game)
          (remainingHand, playedAway) = partitionByIndices (selectedIndices game) (handCards game)
       in case refillHandAfterRemoval remainingHand playedAway (drawPile game) (discardPile game) of
            Just (refilled, newDrawPile, newDiscard) ->
              let updatedGame =
                    game
                      { handCards = refilled
                      , drawPile = newDrawPile
                      , discardPile = newDiscard
                      , selectedIndices = []
                      , handsLeft = handsLeft game - 1
                      , scoreThisBlind = newScore
                      , lastHandScore = handTotalScore
                      , money = money game + moneyGain
                      , playedPreview = selectedCards
                      , scoredPreview = scoreCardsUsed score
                      , previewTimer = 2.2
                      , hoveredCardIndex = Nothing
                      , lastPlayedLabel = describeScoredHand score
                      , message =
                          describeScoredHand score
                            ++ " scored "
                            ++ show handTotalScore
                            ++ pointsSuffix moneyGain
                      }
               in if wonBlind
                    then
                      let interestCash = interestIncome (money updatedGame)
                          payoutCash = blindReward blind + blindBonusCash + jokerGoldCash + interestCash
                       in
                      updatedGame
                        { money = money updatedGame + payoutCash
                        , phase = BlindCleared
                        , message =
                            "Blind cleared with "
                              ++ show newScore
                              ++ "/"
                              ++ show (blindTarget blind)
                              ++ "! Earned $"
                              ++ show (payoutCash + moneyGain)
                              ++ jokerGoldSummary jokerGoldCash
                              ++ interestSummary interestCash
                              ++ "."
                        }
                    else
                      if handsLeft updatedGame <= 0
                        then updatedGame
                              { phase = RunLost
                              , message = "You missed the blind target. Press R for a fresh run."
                              }
                        else updatedGame
            Nothing ->
              let exhaustedGame =
                    game
                      { handCards = remainingHand
                      , drawPile = []
                      , discardPile = discardPile game ++ playedAway
                      , selectedIndices = []
                      , handsLeft = handsLeft game - 1
                      , scoreThisBlind = newScore
                      , lastHandScore = handTotalScore
                      , money = money game + moneyGain
                      , playedPreview = selectedCards
                      , scoredPreview = scoreCardsUsed score
                      , previewTimer = 2.2
                      , hoveredCardIndex = Nothing
                      , lastPlayedLabel = describeScoredHand score
                      }
               in if wonBlind
                    then
                      let interestCash = interestIncome (money exhaustedGame)
                          payoutCash = blindReward blind + blindBonusCash + jokerGoldCash + interestCash
                       in
                      exhaustedGame
                        { money = money exhaustedGame + payoutCash
                        , phase = BlindCleared
                        , message =
                            "Blind cleared with "
                              ++ show newScore
                              ++ "/"
                              ++ show (blindTarget blind)
                              ++ "! Earned $"
                              ++ show (payoutCash + moneyGain)
                              ++ jokerGoldSummary jokerGoldCash
                              ++ interestSummary interestCash
                              ++ "."
                        }
                    else
                      exhaustedGame
                        { phase = RunLost
                        , lastPlayedLabel = "Deck exhausted"
                        , message = "You ran out of cards to draw. Press R for a fresh run."
                        }

advanceBlind :: Game -> Game
advanceBlind game =
  let nextIndex = currentBlindIndex game + 1
   in if nextIndex >= length blinds
        then game
              { phase = RunWon
              , currentBlindIndex = length blinds - 1
              , message = "Boss blind defeated. Press R for another run."
              }
        else openShop (game {currentBlindIndex = nextIndex})

sortHandBy :: (PlayingCard -> PlayingCard -> Ordering) -> Game -> Game
sortHandBy comparator game =
  game
    { handCards = sortBy comparator (handCards game)
    , selectedIndices = []
    , hoveredSortMode = Nothing
    , hoveredCardIndex = Nothing
    }
