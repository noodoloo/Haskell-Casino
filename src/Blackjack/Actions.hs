module Blackjack.Actions
  ( startRound
  , returnToBetting
  , dealIfPossible
  , changePendingBet
  , handleChipClick
  , playerHit
  , playerStand
  , playerSplit
  , playerDoubleDown
  , buyInsurance
  , playerSurrender
  , advanceAfterAction
  ) where

import Blackjack.Cards (drawCard, fullDeck, handTotal, hasFiveCardCharlie, showCard, shuffleDeck)
import Blackjack.Chips (clickedChipValue)
import Blackjack.Settlement (finishDealerTurn, resolveOpeningNaturals)
import Blackjack.State
  ( addHistory
  , canDoubleDownActiveHand
  , canSplitActiveHand
  , clampBet
  , currentPlayerHand
  , hasActiveHand
  , hasEnoughChipsToDouble
  , hasEnoughChipsToSplit
  , markActiveStanding
  , modifyActiveHand
  , nextPlayableHandIndex
  , normalizedBet
  )
import Blackjack.Types
import Blackjack.Utils (formatMoney, replaceAt)

startRound :: Game -> Game
startRound game =
  let (shuffledDeck, gen1) = shuffleDeck fullDeck gen0
      (playerCard1, deck1) = drawCard shuffledDeck
      (dealerCard1, deck2) = drawCard deck1
      (playerCard2, deck3) = drawCard deck2
      (dealerCard2, deck4) = drawCard deck3
      wager = normalizedBet game
      dealtGame =
        game
        { deck = deck4
        , dealerHand = [dealerCard1, dealerCard2]
        , playerHands = [PlayerHand [playerCard1, playerCard2] False wager True]
        , activeHandIndex = 0
        , phase = PlayerTurn
        , chips = chips game - wager
        , pendingBet = wager
        , insuranceBet = Nothing
        , dealTimer = 0.8
        , message =
            if rank dealerCard1 == Ace
              then "Dealer shows Ace. Press I for insurance, U to surrender, or play your hand."
              else "Your turn. H hit, S stand, D double, P split, U surrender."
        , generator = gen1
        }
   in resolveOpeningNaturals dealtGame
  where
    gen0 = generator game

returnToBetting :: Game -> Game
returnToBetting game =
  game
    { deck = []
    , dealerHand = []
    , playerHands = []
    , activeHandIndex = 0
    , phase = Betting
    , pendingBet = normalizedBet game
    , insuranceBet = Nothing
    , dealTimer = 0
    , message = "Choose your bet with Up/Down, then press D to deal."
    }

dealIfPossible :: Game -> Game
dealIfPossible game
  | chips game <= 0 =
      game {message = "You are out of chips. Restart the game to play again."}
  | normalizedBet game > chips game =
      game {message = "Your bet cannot be higher than your current chips."}
  | otherwise = startRound game

changePendingBet :: Double -> Game -> Game
changePendingBet delta game =
  game
    { pendingBet = clampBet (chips game) (pendingBet game + delta)
    , message = "Choose your bet with Up/Down, then press D to deal."
    }

handleChipClick :: (Float, Float) -> Game -> Game
handleChipClick mousePos game =
  case clickedChipValue mousePos of
    Just 0 ->
      game
        { pendingBet = clampBet (chips game) 5
        , message = "Bet reset. Click chips or use Up/Down, then press D to deal."
        }
    Just amount ->
      game
        { pendingBet = clampBet (chips game) (pendingBet game + amount)
        , message = "Added $" ++ formatMoney amount ++ " to the bet. Press D to deal when ready."
        }
    Nothing -> game

playerHit :: Game -> Game
playerHit game
  | phase game /= PlayerTurn = game
  | not (hasActiveHand game) = game
  | otherwise =
      let (newCard, newDeck) = drawCard (deck game)
          updatedHands = modifyActiveHand game (\hand -> hand {cards = cards hand ++ [newCard]})
          activeHand = updatedHands !! activeHandIndex game
          busted = handTotal (cards activeHand) > 21
          charlie = hasFiveCardCharlie activeHand
          nextGame =
            game
              { deck = newDeck
              , playerHands =
                  if busted || charlie
                    then markActiveStanding updatedHands (activeHandIndex game)
                    else updatedHands
              , message =
                  if busted
                    then "Hand " ++ show (activeHandIndex game + 1) ++ " busted after drawing " ++ showCard newCard ++ "."
                    else
                      if charlie
                        then "Hand " ++ show (activeHandIndex game + 1) ++ " made a 5-card Charlie with " ++ showCard newCard ++ "."
                        else "Hand " ++ show (activeHandIndex game + 1) ++ " drew " ++ showCard newCard ++ "."
              , dealTimer = 0.35
              }
       in if busted || charlie then advanceAfterAction nextGame else nextGame

playerStand :: Game -> Game
playerStand game
  | phase game /= PlayerTurn = game
  | not (hasActiveHand game) = game
  | otherwise =
      let updatedHands = markActiveStanding (playerHands game) (activeHandIndex game)
          stoodGame =
            game
              { playerHands = updatedHands
              , message = "Hand " ++ show (activeHandIndex game + 1) ++ " stands."
              }
       in advanceAfterAction stoodGame

playerSplit :: Game -> Game
playerSplit game
  | phase game /= PlayerTurn = game
  | not (hasEnoughChipsToSplit game) =
      game {message = "You need enough chips to match the current hand bet before splitting."}
  | not (canSplitActiveHand game) =
      game {message = "You can only split a hand with exactly two cards of the same rank."}
  | otherwise =
      let hand = playerHands game !! activeHandIndex game
       in case cards hand of
            [firstCard, secondCard] ->
              let (drawnOne, deck1) = drawCard (deck game)
                  (drawnTwo, deck2) = drawCard deck1
                  newHandA = PlayerHand [firstCard, drawnOne] False (betAmount hand) False
                  newHandB = PlayerHand [secondCard, drawnTwo] False (betAmount hand) False
                  updatedHands =
                    replaceAt
                      (activeHandIndex game)
                      [newHandA, newHandB]
                      (playerHands game)
               in game
                    { deck = deck2
                    , chips = chips game - betAmount hand
                    , playerHands = updatedHands
                    , dealTimer = 0.55
                    , message = "Hand split into two hands. The matching bet was added automatically."
                    }
            _ -> game {message = "You can only split a hand with exactly two cards."}

playerDoubleDown :: Game -> Game
playerDoubleDown game
  | phase game /= PlayerTurn = game
  | not (canDoubleDownActiveHand game) =
      game {message = "You can only double down on an active hand that has exactly two cards."}
  | not (hasEnoughChipsToDouble game) =
      game {message = "You need enough chips to match the current hand bet before doubling down."}
  | otherwise =
      let idx = activeHandIndex game
          hand = playerHands game !! idx
          (newCard, newDeck) = drawCard (deck game)
          doubledHand =
            hand
              { cards = cards hand ++ [newCard]
              , isStanding = True
              , betAmount = betAmount hand * 2
              }
          updatedHands =
            take idx (playerHands game) ++ [doubledHand] ++ drop (idx + 1) (playerHands game)
          doubledGame =
            game
              { deck = newDeck
              , chips = chips game - betAmount hand
              , playerHands = updatedHands
              , dealTimer = 0.35
              , message =
                  "Hand "
                    ++ show (idx + 1)
                    ++ " doubled down and drew "
                    ++ showCard newCard
                    ++ "."
              }
       in advanceAfterAction doubledGame

buyInsurance :: Game -> Game
buyInsurance game
  | phase game /= PlayerTurn = game
  | insuranceBet game /= Nothing = game {message = "Insurance is already placed for this round."}
  | not dealerShowsAce = game {message = "Insurance is only available when the dealer shows an Ace."}
  | not openingDecision = game {message = "Insurance is only available before you take another action."}
  | chips game < premium = game {message = "You need $" ++ formatMoney premium ++ " for insurance."}
  | otherwise =
      game
        { chips = chips game - premium
        , insuranceBet = Just premium
        , message = "Insurance placed for $" ++ formatMoney premium ++ "."
        }
  where
    dealerShowsAce =
      case dealerHand game of
        (card : _) -> rank card == Ace
        [] -> False
    openingDecision =
      case playerHands game of
        [hand] -> length (cards hand) == 2 && activeHandIndex game == 0
        _ -> False
    premium = maybe 0 ((/ 2) . betAmount) (currentPlayerHand game)

playerSurrender :: Game -> Game
playerSurrender game
  | phase game /= PlayerTurn = game
  | not canSurrender = game {message = "Surrender is only available before taking another action."}
  | otherwise =
      let hand = playerHands game !! activeHandIndex game
          refund = betAmount hand / 2
          summaryLine = "Surrendered and got $" ++ formatMoney refund ++ " back."
       in game
            { phase = RoundFinished
            , activeHandIndex = 0
            , chips = chips game + refund
            , message = summaryLine ++ " Press N for the next betting round."
            , history = addHistory summaryLine (history game)
            }
  where
    canSurrender =
      case playerHands game of
        [hand] -> activeHandIndex game == 0 && length (cards hand) == 2 && blackjackEligible hand
        _ -> False

advanceAfterAction :: Game -> Game
advanceAfterAction game =
  case nextPlayableHandIndex (playerHands game) (activeHandIndex game + 1) of
    Just idx -> game {activeHandIndex = idx}
    Nothing -> finishDealerTurn game
