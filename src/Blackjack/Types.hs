module Blackjack.Types
  ( Suit (..)
  , Rank (..)
  , Card (..)
  , PlayerHand (..)
  , RoundPhase (..)
  , Game (..)
  ) where

import System.Random (StdGen)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Enum, Bounded)

data Rank
  = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace
  deriving (Eq, Enum, Bounded)

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Eq)

data PlayerHand = PlayerHand
  { cards :: [Card]
  , isStanding :: Bool
  , betAmount :: Double
  , blackjackEligible :: Bool
  } deriving (Eq)

data RoundPhase = Betting | PlayerTurn | RoundFinished
  deriving (Eq)

data Game = Game
  { deck :: [Card]
  , dealerHand :: [Card]
  , playerHands :: [PlayerHand]
  , activeHandIndex :: Int
  , phase :: RoundPhase
  , chips :: Double
  , pendingBet :: Double
  , insuranceBet :: Maybe Double
  , dealTimer :: Float
  , history :: [String]
  , message :: String
  , generator :: StdGen
  }
