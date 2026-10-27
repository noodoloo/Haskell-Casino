module Blackjack.Game
  ( Game
  , initialGame
  , initialGameWithChips
  , blackjackBankroll
  , setBlackjackBankroll
  , runBlackjackApp
  , renderGame
  , handleEvent
  , stepGame
  ) where

import Graphics.Gloss
import System.Random (newStdGen)

import Blackjack.Input (handleEvent)
import Blackjack.Render (renderGame)
import Blackjack.State (clampBet, initialLobby)
import Blackjack.Types

initialGame :: IO Game
initialGame = do
  gen <- newStdGen
  pure (initialLobby gen)

initialGameWithChips :: Double -> IO Game
initialGameWithChips startingChips = do
  gen <- newStdGen
  pure (initialLobby gen) {chips = startingChips, pendingBet = clampBet startingChips 25}

blackjackBankroll :: Game -> Double
blackjackBankroll game =
  case phase game of
    PlayerTurn -> chips game + sum (map betAmount (playerHands game)) + maybe 0 id (insuranceBet game)
    _ -> chips game

setBlackjackBankroll :: Double -> Game -> Game
setBlackjackBankroll amount game =
  game {chips = amount, pendingBet = clampBet amount (pendingBet game)}

runBlackjackApp :: IO ()
runBlackjackApp = do
  game <- initialGame
  play
    (InWindow "Haskell Casino - Blackjack" (1200, 680) (100, 80))
    black
    60
    game
    renderGame
    handleEvent
    stepGame

stepGame :: Float -> Game -> Game
stepGame dt game =
  game {dealTimer = max 0 (dealTimer game - dt)}
