module Roulette.Types
    ( BetType(..)
    , Bet(..)
    , SpinResult(..)
    , GameState(..)
    , initialState
    , boardNumbers
    ) where

data BetType
    = StraightUp Int
    | Red
    | Black
    | Even
    | Odd
    | Low
    | High
    deriving (Eq, Show)

data Bet = Bet
    { betType :: BetType
    , amount  :: Int
    }
    deriving (Eq, Show)

data SpinResult = SpinResult
    { rolledNumber :: Int
    , wasRed       :: Bool
    }
    deriving (Eq, Show)

data GameState = GameState
    { balance    :: Int
    , currentBet :: Maybe Bet
    , lastSpin   :: Maybe SpinResult
    , message    :: String
    }
    deriving (Eq, Show)

initialState :: GameState
initialState = GameState
    { balance = 500
    , currentBet = Nothing
    , lastSpin = Nothing
    , message = "Press 1-6 to place a bet, Enter to spin."
    }

boardNumbers :: [[Int]]
boardNumbers =
    [ [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36]
    , [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35]
    , [1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34]
    ]
