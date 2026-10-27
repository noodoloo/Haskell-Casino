module Blackjack.State
  ( initialLobby
  , hasActiveHand
  , hasEnoughChipsToSplit
  , hasEnoughChipsToDouble
  , addHistory
  , currentPlayerHand
  , modifyActiveHand
  , markActiveStanding
  , nextPlayableHandIndex
  , canSplitActiveHand
  , canDoubleDownActiveHand
  , normalizedBet
  , clampBet
  ) where

import System.Random (StdGen)

import Blackjack.Cards (handTotal)
import Blackjack.Types

initialLobby :: StdGen -> Game
initialLobby gen =
  Game
    { deck = []
    , dealerHand = []
    , playerHands = []
    , activeHandIndex = 0
    , phase = Betting
    , chips = 500
    , pendingBet = 25
    , insuranceBet = Nothing
    , dealTimer = 0
    , history = []
    , message = "Choose your bet with Up/Down, then press Enter to deal."
    , generator = gen
    }

hasActiveHand :: Game -> Bool
hasActiveHand game =
  activeHandIndex game >= 0 && activeHandIndex game < length (playerHands game)

hasEnoughChipsToSplit :: Game -> Bool
hasEnoughChipsToSplit game =
  case currentPlayerHand game of
    Just hand -> chips game >= betAmount hand
    Nothing -> False

hasEnoughChipsToDouble :: Game -> Bool
hasEnoughChipsToDouble game =
  case currentPlayerHand game of
    Just hand -> chips game >= betAmount hand
    Nothing -> False

addHistory :: String -> [String] -> [String]
addHistory entry entries = take 5 (entry : entries)

currentPlayerHand :: Game -> Maybe PlayerHand
currentPlayerHand game
  | hasActiveHand game = Just (playerHands game !! activeHandIndex game)
  | otherwise = Nothing

modifyActiveHand :: Game -> (PlayerHand -> PlayerHand) -> [PlayerHand]
modifyActiveHand game f =
  let idx = activeHandIndex game
   in take idx (playerHands game) ++ [f (playerHands game !! idx)] ++ drop (idx + 1) (playerHands game)

markActiveStanding :: [PlayerHand] -> Int -> [PlayerHand]
markActiveStanding hands idx =
  take idx hands
    ++ [ (hands !! idx) {isStanding = True} ]
    ++ drop (idx + 1) hands

nextPlayableHandIndex :: [PlayerHand] -> Int -> Maybe Int
nextPlayableHandIndex hands startIdx =
  case filter (\idx -> not (isStanding (hands !! idx)) && handTotal (cards (hands !! idx)) <= 21) [startIdx .. length hands - 1] of
    idx : _ -> Just idx
    [] -> Nothing

canSplitActiveHand :: Game -> Bool
canSplitActiveHand game =
  case currentPlayerHand game of
    Just hand ->
      case cards hand of
        [cardA, cardB] -> rank cardA == rank cardB
        _ -> False
    Nothing -> False

canDoubleDownActiveHand :: Game -> Bool
canDoubleDownActiveHand game =
  case currentPlayerHand game of
    Just hand -> not (isStanding hand) && length (cards hand) == 2
    Nothing -> False

normalizedBet :: Game -> Double
normalizedBet game = clampBet (chips game) (pendingBet game)

clampBet :: Double -> Double -> Double
clampBet available requested
  | available <= 0 = 0
  | available < 5 = available
  | otherwise = max 5 (min available requested)
