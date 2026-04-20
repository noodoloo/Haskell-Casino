module Main where

import Blackjack (runBlackjackApp)
import Roulette.UI (runRouletteApp)
import Slots.UI (runSlotsApp)

main :: IO ()
main = do
    putStrLn "Haskell Casino"
    putStrLn "1. Blackjack"
    putStrLn "2. Roulette"
    putStrLn "3. Slots"
    putStrLn "Choose a game:"
    choice <- getLine
    case choice of
        "1" -> runBlackjackApp
        "blackjack" -> runBlackjackApp
        "Blackjack" -> runBlackjackApp
        "2" -> runRouletteApp
        "roulette" -> runRouletteApp
        "Roulette" -> runRouletteApp
        "3" -> runSlotsApp
        "slots" -> runSlotsApp
        "Slots" -> runSlotsApp
        _ -> do
            putStrLn "Please choose 1, 2, or 3."
            main
