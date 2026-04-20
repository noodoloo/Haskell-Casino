module Slots.Logic
    ( spinSlots
    , changeBet
    , maxBet
    , resetSlots
    , scoreReels
    ) where

import System.Random (randomRIO)

import Slots.Types

weightedSymbols :: [Symbol]
weightedSymbols =
    [ Cherry
    , Cherry
    , Cherry
    , Lemon
    , Lemon
    , Lemon
    , Bell
    , Bell
    , Bar
    , Bar
    , Seven
    , Diamond
    ]

spinSlots :: GameState -> IO GameState
spinSlots st
    | balance st <= 0 =
        pure st {message = "You are out of chips. Press R to reset your bankroll."}
    | currentBet st > balance st =
        pure st {message = "Lower your bet before spinning."}
    | otherwise = do
        resultSymbols <- sequence [randomSymbol, randomSymbol, randomSymbol]
        let mult = scoreReels resultSymbols
            winnings = currentBet st * mult
            newBalance = balance st - currentBet st + winnings
            result =
                SpinResult
                    { reels = resultSymbols
                    , multiplier = mult
                    , payout = winnings
                    }
        pure
            st
                { balance = newBalance
                , lastSpin = Just result
                , message = spinMessage result
                }

changeBet :: Int -> GameState -> GameState
changeBet direction st =
    let currentIndex = maybe 0 id (findIndexOf (currentBet st) betSteps)
        nextIndex = clamp 0 (length betSteps - 1) (currentIndex + direction)
        nextBet =
            if balance st <= 0
                then 0
                else min (betSteps !! nextIndex) (max 5 (balance st))
     in st
            { currentBet = nextBet
            , message = "Bet set to $" ++ show nextBet ++ "."
            }

maxBet :: GameState -> GameState
maxBet st =
    let affordable = filter (<= balance st) betSteps
        nextBet =
            case affordable of
                [] -> balance st
                _ -> last affordable
     in st
            { currentBet = nextBet
            , message = "Max bet set to $" ++ show nextBet ++ "."
            }

resetSlots :: GameState
resetSlots = initialState {message = "Bankroll reset. Press Space or Enter to spin."}

scoreReels :: [Symbol] -> Int
scoreReels [Seven, Seven, Seven] = 20
scoreReels [Diamond, Diamond, Diamond] = 15
scoreReels [Bar, Bar, Bar] = 10
scoreReels [Bell, Bell, Bell] = 8
scoreReels [a, b, c]
    | a == b && b == c = 5
    | cherryCount >= 2 = 2
    | cherryCount == 1 = 1
    | otherwise = 0
  where
    cherryCount = length (filter (== Cherry) [a, b, c])
scoreReels _ = 0

randomSymbol :: IO Symbol
randomSymbol = do
    idx <- randomRIO (0, length weightedSymbols - 1)
    pure (weightedSymbols !! idx)

spinMessage :: SpinResult -> String
spinMessage result
    | multiplier result == 0 =
        "No match this time. Spin again when ready."
    | otherwise =
        "Won $"
            ++ show (payout result)
            ++ " with "
            ++ show (multiplier result)
            ++ "x payout."

findIndexOf :: Eq a => a -> [a] -> Maybe Int
findIndexOf target = go 0
  where
    go _ [] = Nothing
    go idx (x : xs)
        | x == target = Just idx
        | otherwise = go (idx + 1) xs

clamp :: Int -> Int -> Int -> Int
clamp low high value = max low (min high value)
