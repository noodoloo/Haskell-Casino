module Blackjack.Settlement
  ( finishDealerTurn
  , playDealer
  , dealerShouldHit
  , describeOutcome
  , settlementForHand
  , resolveOpeningNaturals
  , insurancePayout
  , describeInsurance
  ) where

import Blackjack.Cards (drawCard, handTotal, hasFiveCardCharlie, hasNaturalBlackjack, isSoft)
import Blackjack.State (addHistory)
import Blackjack.Types
import Blackjack.Utils (formatMoney)

finishDealerTurn :: Game -> Game
finishDealerTurn game =
  let finalDealer = playDealer (deck game) (dealerHand game)
      finalHands = playerHands game
      payout = sum (map (settlementForHand finalDealer) finalHands) + insurancePayout game finalDealer
      resultLines = zipWith (describeOutcome finalDealer) [1 ..] finalHands
      insuranceLine = describeInsurance game finalDealer
      roundSummary = unwords (resultLines ++ insuranceLine)
   in game
        { dealerHand = finalDealer
        , phase = RoundFinished
        , activeHandIndex = 0
        , chips = chips game + payout
        , message = roundSummary ++ " Press N for the next betting round."
        , history = addHistory roundSummary (history game)
        }

playDealer :: [Card] -> [Card] -> [Card]
playDealer currentDeck currentHand
  | dealerShouldHit currentHand =
      let (nextCard, remainingDeck) = drawCard currentDeck
       in playDealer remainingDeck (currentHand ++ [nextCard])
  | otherwise = currentHand

dealerShouldHit :: [Card] -> Bool
dealerShouldHit hand =
  let total = handTotal hand
   in total < 17 || (total == 17 && isSoft hand)

describeOutcome :: [Card] -> Int -> PlayerHand -> String
describeOutcome dealer idx hand
  | hasNaturalBlackjack hand && hasNaturalBlackjack dealerHandState = "Hand " ++ show idx ++ " pushes with blackjack."
  | hasNaturalBlackjack hand = "Hand " ++ show idx ++ " has blackjack and wins $" ++ formatMoney (wager * 1.5) ++ "."
  | hasFiveCardCharlie hand = "Hand " ++ show idx ++ " wins with a 5-card Charlie."
  | playerScore > 21 = "Hand " ++ show idx ++ " loses $" ++ formatMoney wager ++ "."
  | dealerScore > 21 = "Hand " ++ show idx ++ " wins $" ++ formatMoney wager ++ "."
  | hasNaturalBlackjack dealerHandState = "Hand " ++ show idx ++ " loses to dealer blackjack."
  | playerScore > dealerScore = "Hand " ++ show idx ++ " wins $" ++ formatMoney wager ++ "."
  | playerScore < dealerScore = "Hand " ++ show idx ++ " loses $" ++ formatMoney wager ++ "."
  | otherwise = "Hand " ++ show idx ++ " pushes."
  where
    playerScore = handTotal (cards hand)
    dealerScore = handTotal dealer
    wager = betAmount hand
    dealerHandState = PlayerHand dealer False 0 False

settlementForHand :: [Card] -> PlayerHand -> Double
settlementForHand dealer hand
  | hasNaturalBlackjack hand && hasNaturalBlackjack dealerHandState = wager
  | hasNaturalBlackjack hand = wager * 2.5
  | hasFiveCardCharlie hand = wager * 2
  | playerScore > 21 = 0
  | hasNaturalBlackjack dealerHandState = 0
  | dealerScore > 21 = wager * 2
  | playerScore > dealerScore = wager * 2
  | playerScore < dealerScore = 0
  | otherwise = wager
  where
    playerScore = handTotal (cards hand)
    dealerScore = handTotal dealer
    wager = betAmount hand
    dealerHandState = PlayerHand dealer False 0 False

resolveOpeningNaturals :: Game -> Game
resolveOpeningNaturals game
  | dealerNatural && dealerShowsAce && not playerNatural =
      game {message = "Dealer shows Ace. Press I for insurance before playing your hand."}
  | playerNatural || dealerNatural =
      let finalHands = playerHands game
          finalDealer = dealerHand game
          payout = sum (map (settlementForHand finalDealer) finalHands) + insurancePayout game finalDealer
          resultLines = zipWith (describeOutcome finalDealer) [1 ..] finalHands
          insuranceLine = describeInsurance game finalDealer
          roundSummary = unwords (resultLines ++ insuranceLine)
       in game
            { phase = RoundFinished
            , chips = chips game + payout
            , message = roundSummary ++ " Press N for the next betting round."
            , history = addHistory roundSummary (history game)
            }
  | otherwise = game
  where
    playerNatural = any hasNaturalBlackjack (playerHands game)
    dealerNatural = hasNaturalBlackjack (PlayerHand (dealerHand game) False 0 False)
    dealerShowsAce =
      case dealerHand game of
        (card : _) -> rank card == Ace
        [] -> False

insurancePayout :: Game -> [Card] -> Double
insurancePayout game finalDealer =
  case insuranceBet game of
    Just premium
      | hasNaturalBlackjack (PlayerHand finalDealer False 0 False) -> premium * 3
    _ -> 0

describeInsurance :: Game -> [Card] -> [String]
describeInsurance game finalDealer =
  case insuranceBet game of
    Nothing -> []
    Just premium
      | hasNaturalBlackjack (PlayerHand finalDealer False 0 False) ->
          ["Insurance pays $" ++ formatMoney (premium * 2) ++ "."]
      | otherwise ->
          ["Insurance loses $" ++ formatMoney premium ++ "."]
