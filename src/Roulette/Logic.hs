module Roulette.Logic
    ( redNumbers
    , resolveBet
    , spinWheel
    , placeBet
    , cycleChip
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
    | amount bet > availableBalance st =
        st {message = "Not enough balance for that bet."}
    | otherwise =
        let updatedBets = addOrMergeBet bet (activeBets st)
         in st
                { activeBets = updatedBets
                , message = "Chip placed. Press Enter to spin or keep betting."
                }

cycleChip :: GameState -> GameState
cycleChip st =
    let nextChip =
            case dropWhile (/= selectedChip st) chipValues of
                (_ : x : _) -> x
                _ -> head chipValues
     in st
            { selectedChip = nextChip
            , message = "Selected chip: $" ++ show nextChip
            }

applySpin :: SpinResult -> GameState -> GameState
applySpin result st =
    case activeBets st of
        [] ->
            st {message = "Place a bet before spinning."}
        bets ->
            let delta = sum (map (`resolveBet` result) bets)
                newBalance = balance st + delta
                outcomeText
                    | delta >= 0 = "You won $" ++ show delta ++ "."
                    | otherwise = "You lost $" ++ show (abs delta) ++ "."
             in st
                    { balance = newBalance
                    , activeBets = []
                    , lastSpin = Just result
                    , message =
                        "Rolled "
                            ++ show (rolledNumber result)
                            ++ ". "
                            ++ outcomeText
                    }

availableBalance :: GameState -> Int
availableBalance st = balance st - sum (map amount (activeBets st))

addOrMergeBet :: Bet -> [Bet] -> [Bet]
addOrMergeBet bet [] = [bet]
addOrMergeBet bet (existing : rest)
    | betType existing == betType bet =
        existing {amount = amount existing + amount bet} : rest
    | otherwise =
        existing : addOrMergeBet bet rest
