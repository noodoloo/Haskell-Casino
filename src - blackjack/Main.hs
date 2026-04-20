module Main where

import Blackjack
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  game <- initialGame
  play
    (InWindow "Haskell Casino - Blackjack" (1200, 680) (100, 80))
    black
    60
    game
    renderGame
    handleEvent
    stepGame
