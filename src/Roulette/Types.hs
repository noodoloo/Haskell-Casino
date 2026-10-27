module Roulette.Types
    ( BetType(..)
    , Bet(..)
    , SpinResult(..)
    , GameState(..)
    , chipValues
    , initialState
    , boardNumbers
    ) where

data BetType
    = StraightUp Int
    | Corner [Int]
    | Column Int
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
    , activeBets :: [Bet]
    , selectedChip :: Int
    , lastSpin   :: Maybe SpinResult
    , pendingSpin :: Maybe SpinResult
    , isSpinning :: Bool
    , spinTimer :: Float
    , wheelRotation :: Float
    , ballRotation :: Float
    , message    :: String
    }
    deriving (Eq, Show)

chipValues :: [Int]
chipValues = [5, 10, 25, 50]

initialState :: GameState
initialState = GameState
    { balance = 500
    , activeBets = []
    , selectedChip = 25
    , lastSpin = Nothing
    , pendingSpin = Nothing
    , isSpinning = False
    , spinTimer = 0
    , wheelRotation = 0
    , ballRotation = pi / 2
    , message = "Click the table to place chips. Press C to change chip, Enter to spin."
    }

boardNumbers :: [[Int]]
boardNumbers =
    [ [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36]
    , [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35]
    , [1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34]
    ]
