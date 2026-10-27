module BalatroMini.Types
  ( Suit (..)
  , Rank (..)
  , Enchant (..)
  , PlayingCard (..)
  , JokerId (..)
  , Joker (..)
  , BlindKind (..)
  , Blind (..)
  , GamePhase (..)
  , ShopItem (..)
  , HandType (..)
  , SortMode (..)
  , HandScore (..)
  , Game (..)
  , ScoreBreakdown (..)
  ) where

import System.Random (StdGen)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show)

data Rank
  = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show)

data Enchant
  = NoEnchant
  | BonusChips
  | LuckyMult
  | TimesMult
  | GoldSeal
  | WildSuit
  deriving (Eq)

data PlayingCard = PlayingCard
  { cardRank :: Rank
  , cardSuit :: Suit
  , cardEnchant :: Enchant
  } deriving (Eq)

data JokerId
  = FlatMultJoker
  | PairPrinter
  | FaceCollector
  | GreedyJoker
  | StraightShooter
  | BlindPurse
  | ChipSprinter
  | TwinPair
  | AcePilot
  | ExtraDiscard
  | ExtraHand
  | EmptyStage
  | SuitPainter
  | ShortcutArtist
  | EnhancementEngine
  deriving (Eq, Enum, Bounded)

data Joker = Joker
  { jokerId :: JokerId
  , jokerName :: String
  , jokerText :: String
  , jokerCost :: Int
  , jokerEnchant :: Enchant
  }

data BlindKind = SmallBlind | BigBlind | BossBlind
  deriving (Eq)

data Blind = Blind
  { blindAnte :: Int
  , blindKind :: BlindKind
  , blindTarget :: Int
  , blindReward :: Int
  }

data GamePhase
  = PlayingBlind
  | BlindCleared
  | Shopping
  | RunWon
  | RunLost
  deriving (Eq)

data ShopItem
  = ShopJoker Joker
  | ShopCard PlayingCard Int
  | ShopUpgrade HandType Int

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  | RoyalFlush
  | FiveKind
  | FlushHouse
  | FlushFive
  deriving (Eq, Enum, Bounded)

data SortMode = SortByRank | SortBySuit
  deriving (Eq)

data HandScore = HandScore
  { handType :: HandType
  , baseChips :: Int
  , baseMult :: Int
  }
-- All book-keeping for the game state
data Game = Game
  { permanentDeck :: [PlayingCard]
  , drawPile :: [PlayingCard]
  , handCards :: [PlayingCard]
  , discardPile :: [PlayingCard]
  , playedPreview :: [PlayingCard]
  , scoredPreview :: [PlayingCard]
  , previewTimer :: Float
  , selectedIndices :: [Int]
  , jokers :: [Joker]
  , handUpgrades :: [(HandType, Int)]
  , currentBlindIndex :: Int
  , scoreThisBlind :: Int
  , lastHandScore :: Int
  , handsLeft :: Int
  , discardsLeft :: Int
  , money :: Int
  , phase :: GamePhase
  , shopItems :: [ShopItem]
  , removalsLeft :: Int
  , shopDeckCursor :: Int
  , hoveredSortMode :: Maybe SortMode
  , hoveredCardIndex :: Maybe Int
  , expandedJokerIndex :: Maybe Int
  , confirmSellJokerIndex :: Maybe Int
  , showHandGuide :: Bool
  , lastPlayedLabel :: String
  , message :: String
  , generator :: StdGen
  }

data ScoreBreakdown = ScoreBreakdown
  { scoreBase :: HandScore
  , scoreCardsUsed :: [PlayingCard]
  , scoreChipBonus :: Int
  , scoreMultBonus :: Int
  , scoreMultFactor :: Float
  , scoreMoneyGain :: Int
  , scoreNotes :: [String]
  }
