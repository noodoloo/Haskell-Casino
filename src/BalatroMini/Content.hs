module BalatroMini.Content
  ( blinds
  , totalAntes
  , jokerPool
  , blindLabel
  , handTypeName
  , upgradeChipsPerLevel
  , upgradeMultPerLevel
  , upgradeHandPool
  , pokerHandGuideRows
  ) where

import BalatroMini.Types

blinds :: [Blind]
blinds =
  [ Blind 1 SmallBlind 300 2
  , Blind 1 BigBlind 500 3
  , Blind 2 SmallBlind 850 3
  , Blind 2 BigBlind 1250 4
  , Blind 3 SmallBlind 1800 4
  , Blind 3 BigBlind 2500 5
  , Blind 4 SmallBlind 3400 5
  , Blind 4 BigBlind 4550 6
  , Blind 5 SmallBlind 6000 6
  , Blind 5 BigBlind 7800 7
  , Blind 5 BossBlind 10000 9
  ]

totalAntes :: Int
totalAntes =
  maximum (map blindAnte blinds)

jokerPool :: [Joker]
jokerPool =
  [ Joker FlatMultJoker "Joker" "+5 mult every hand." 4 NoEnchant
  , Joker PairPrinter "Pair Printer" "+40 chips if hand has a pair." 6 NoEnchant
  , Joker FaceCollector "Face Collector" "+2 mult per face card played." 6 NoEnchant
  , Joker GreedyJoker "Greedy Joker" "+2 mult per red suit played." 6 NoEnchant
  , Joker StraightShooter "Straight Shooter" "+80 chips on straights." 7 NoEnchant
  , Joker BlindPurse "Blind Purse" "Gain $3 per blind." 5 NoEnchant
  , Joker ChipSprinter "Chip Sprinter" "+25 chips every hand." 4 NoEnchant
  , Joker TwinPair "Twin Pair" "+6 mult on Two Pair." 5 NoEnchant
  , Joker AcePilot "Ace Pilot" "+4 mult per Ace played." 6 NoEnchant
  , Joker ExtraDiscard "Loose Sleeve" "+1 discard each blind." 5 NoEnchant
  , Joker ExtraHand "Encore" "+1 hand each blind." 6 NoEnchant
  , Joker EmptyStage "Empty Stage" "X mult equal to empty joker slots." 7 NoEnchant
  , Joker SuitPainter "Suit Painter" "Red suits match; black suits match." 6 NoEnchant
  , Joker ShortcutArtist "Shortcut Artist" "Straights and flushes only need 4 cards." 7 NoEnchant
  , Joker EnhancementEngine "Enhancement Engine" "x0.25 mult per enhanced deck card." 8 NoEnchant
  ]

blindLabel :: BlindKind -> String
blindLabel kind =
  case kind of
    SmallBlind -> "Small Blind"
    BigBlind -> "Big Blind"
    BossBlind -> "Boss Blind"

handTypeName :: HandType -> String
handTypeName handKind =
  case handKind of
    HighCard -> "High Card"
    OnePair -> "Pair"
    TwoPair -> "Two Pair"
    ThreeKind -> "Three of a Kind"
    Straight -> "Straight"
    Flush -> "Flush"
    FullHouse -> "Full House"
    FourKind -> "Four of a Kind"
    StraightFlush -> "Straight Flush"
    RoyalFlush -> "Royal Flush"
    FiveKind -> "Five of a Kind"
    FlushHouse -> "Flush House"
    FlushFive -> "Flush Five"

upgradeChipsPerLevel :: Int
upgradeChipsPerLevel = 10

upgradeMultPerLevel :: Int
upgradeMultPerLevel = 1

upgradeHandPool :: [HandType]
upgradeHandPool =
  [ HighCard
  , OnePair
  , TwoPair
  , ThreeKind
  , Straight
  , Flush
  , FullHouse
  , FourKind
  , StraightFlush
  , RoyalFlush
  , FiveKind
  , FlushHouse
  , FlushFive
  ]

pokerHandGuideRows :: [(HandType, Int, Int)]
pokerHandGuideRows =
  [ (FlushFive, 180, 16)
  , (FlushHouse, 160, 14)
  , (FiveKind, 140, 12)
  , (RoyalFlush, 120, 10)
  , (StraightFlush, 100, 8)
  , (FourKind, 60, 7)
  , (FullHouse, 45, 4)
  , (Flush, 35, 4)
  , (Straight, 30, 4)
  , (ThreeKind, 35, 3)
  , (TwoPair, 25, 2)
  , (OnePair, 15, 2)
  , (HighCard, 5, 1)
  ]
