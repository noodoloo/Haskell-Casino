module Roulette.Logic
    ( redNumbers
    , resolveBet
    , spinWheel
    , placeBet
    , applySpin
    ) where

import System.Random (randomRIO)

import Roulette.Types

redNumbers :: [Int]
redNumbers =
    [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

spinWheel :: IO SpinResult
spinWheel = do
    n <- randomRIO (0, 36)
    pure SpinResult
        { rolledNumber = n
        , wasRed = n `elem` redNumbers
        }

resolveBet :: Bet -> SpinResult -> Int
resolveBet bet result =
    let n = rolledNumber result
        wager = amount bet
     in case betType bet of
            StraightUp chosen ->
                if n == chosen then wager * 35 else negate wager
            Red ->
                if n /= 0 && wasRed result then wager else negate wager
            Black ->
                if n /= 0 && not (wasRed result) then wager else negate wager
            Even ->
                if n /= 0 && even n then wager else negate wager
            Odd ->
                if odd n then wager else negate wager
            Low ->
                if n >= 1 && n <= 18 then wager else negate wager
            High ->
                if n >= 19 && n <= 36 then wager else negate wager

placeBet :: Bet -> GameState -> GameState
placeBet bet st
    | amount bet <= 0 =
        st {message = "Bet amount must be positive."}
    | amount bet > balance st =
        st {message = "Not enough balance for that bet."}
    | otherwise =
        st
            { currentBet = Just bet
            , message = "Bet locked in. Press Enter to spin."
            }

applySpin :: SpinResult -> GameState -> GameState
applySpin result st =
    case currentBet st of
        Nothing ->
            st {message = "Place a bet before spinning."}
        Just bet ->
            let delta = resolveBet bet result
                newBalance = balance st + delta
                outcomeText
                    | delta >= 0 = "You won $" ++ show delta ++ "."
                    | otherwise = "You lost $" ++ show (abs delta) ++ "."
             in st
                    { balance = newBalance
                    , currentBet = Nothing
                    , lastSpin = Just result
                    , message =
                        "Rolled "
                            ++ show (rolledNumber result)
                            ++ ". "
                            ++ outcomeText
                    }
