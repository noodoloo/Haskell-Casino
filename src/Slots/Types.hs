module Slots.Types
    ( Symbol(..)
    , SpinResult(..)
    , GameState(..)
    , symbols
    , betSteps
    , initialState
    , symbolName
    ) where

data Symbol
    = Cherry
    | Lemon
    | Bell
    | Bar
    | Seven
    | Diamond
    deriving (Eq, Show)

data SpinResult = SpinResult
    { reels :: [Symbol]
    , multiplier :: Int
    , payout :: Int
    }
    deriving (Eq, Show)

data GameState = GameState
    { balance :: Int
    , currentBet :: Int
    , lastSpin :: Maybe SpinResult
    , message :: String
    }
    deriving (Eq, Show)

symbols :: [Symbol]
symbols = [Cherry, Lemon, Bell, Bar, Seven, Diamond]

betSteps :: [Int]
betSteps = [5, 10, 25, 50, 100]

initialState :: GameState
initialState =
    GameState
        { balance = 500
        , currentBet = 25
        , lastSpin = Nothing
        , message = "Press Space or Enter to spin. Use Up/Down to change your bet."
        }

symbolName :: Symbol -> String
symbolName symbol =
    case symbol of
        Cherry -> "CHERRY"
        Lemon -> "LEMON"
        Bell -> "BELL"
        Bar -> "BAR"
        Seven -> "SEVEN"
        Diamond -> "DIAMOND"
