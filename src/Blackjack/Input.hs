module Blackjack.Input
  ( handleEvent
  ) where

import Graphics.Gloss.Interface.Pure.Game

import Blackjack.Actions
  ( buyInsurance
  , changePendingBet
  , dealIfPossible
  , handleChipClick
  , playerDoubleDown
  , playerHit
  , playerSplit
  , playerStand
  , playerSurrender
  , returnToBetting
  )
import Blackjack.Types

handleEvent :: Event -> Game -> Game
handleEvent event game =
  case phase game of
    Betting -> handleBettingEvent event game
    PlayerTurn -> handlePlayerTurnEvent event game
    RoundFinished -> handleRoundFinishedEvent event game

handleBettingEvent :: Event -> Game -> Game
handleBettingEvent event game =
  case event of
    EventKey (MouseButton LeftButton) Down _ mousePos -> handleChipClick mousePos game
    EventKey (SpecialKey KeyUp) Down _ _ -> changePendingBet 5 game
    EventKey (SpecialKey KeyDown) Down _ _ -> changePendingBet (-5) game
    EventKey (Char '+') Down _ _ -> changePendingBet 5 game
    EventKey (Char '-') Down _ _ -> changePendingBet (-5) game
    EventKey (Char 'd') Down _ _ -> dealIfPossible game
    EventKey (Char 'D') Down _ _ -> dealIfPossible game
    EventKey (SpecialKey KeyEnter) Down _ _ -> dealIfPossible game
    EventKey (SpecialKey KeySpace) Down _ _ -> dealIfPossible game
    _ -> game

handlePlayerTurnEvent :: Event -> Game -> Game
handlePlayerTurnEvent event game =
  case event of
    EventKey (Char 'h') Down _ _ -> playerHit game
    EventKey (Char 'H') Down _ _ -> playerHit game
    EventKey (Char 's') Down _ _ -> playerStand game
    EventKey (Char 'S') Down _ _ -> playerStand game
    EventKey (Char 'd') Down _ _ -> playerDoubleDown game
    EventKey (Char 'D') Down _ _ -> playerDoubleDown game
    EventKey (Char 'i') Down _ _ -> buyInsurance game
    EventKey (Char 'I') Down _ _ -> buyInsurance game
    EventKey (Char 'p') Down _ _ -> playerSplit game
    EventKey (Char 'P') Down _ _ -> playerSplit game
    EventKey (Char 'u') Down _ _ -> playerSurrender game
    EventKey (Char 'U') Down _ _ -> playerSurrender game
    EventKey (SpecialKey KeySpace) Down _ _ -> playerStand game
    _ -> game

handleRoundFinishedEvent :: Event -> Game -> Game
handleRoundFinishedEvent event game =
  case event of
    EventKey (Char 'n') Down _ _ -> returnToBetting game
    EventKey (Char 'N') Down _ _ -> returnToBetting game
    EventKey (Char 'd') Down _ _ -> dealIfPossible (returnToBetting game)
    EventKey (Char 'D') Down _ _ -> dealIfPossible (returnToBetting game)
    _ -> game
