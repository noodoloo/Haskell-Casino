module BalatroMini
  ( Game
  , balatroStartingMoney
  , balatroCasinoPayout
  , startFreshGame
  , renderGame
  , handleEvent
  , stepGame
  ) where

import Data.List (group, intercalate, sort, sortBy, sortOn)
import Data.Ord (comparing)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (RandomGen, StdGen, mkStdGen, randomR)

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

startFreshGame :: Int -> Game
startFreshGame seed =
  let gen0 = mkStdGen (seed + 4242)
      initialDeck = [PlayingCard r s NoEnchant | s <- [minBound .. maxBound], r <- [minBound .. maxBound]]
      (shuffled, gen1) = shuffleList initialDeck gen0
      openingHand = drawUpTo 8 shuffled []
      drawn = fst openingHand
      rest = snd openingHand
   in Game
        { permanentDeck = initialDeck
        , drawPile = rest
        , handCards = drawn
        , discardPile = []
        , playedPreview = []
        , scoredPreview = []
        , previewTimer = 0
        , selectedIndices = []
        , jokers = []
        , handUpgrades = []
        , currentBlindIndex = 0
        , scoreThisBlind = 0
        , lastHandScore = 0
        , handsLeft = 4
        , discardsLeft = 3
        , money = 3
        , phase = PlayingBlind
        , shopItems = []
        , removalsLeft = 0
        , shopDeckCursor = 0
        , hoveredSortMode = Nothing
        , hoveredCardIndex = Nothing
        , expandedJokerIndex = Nothing
        , confirmSellJokerIndex = Nothing
        , showHandGuide = False
        , lastPlayedLabel = "Play a hand"
        , message = "Mini Balatro: select cards with 1-8, Enter plays, D discards."
        , generator = gen1
        }

balatroStartingMoney :: Int
balatroStartingMoney = 3

balatroCasinoPayout :: Game -> Double
balatroCasinoPayout game = fromIntegral (max 0 (money game - balatroStartingMoney))

stepGame :: Float -> Game -> Game
stepGame dt game
  | phase game == BlindCleared = game
  | previewTimer game <= 0 = game
  | nextTimer <= 0 =
      game
        { playedPreview = []
        , scoredPreview = []
        , previewTimer = 0
        }
  | otherwise = game {previewTimer = nextTimer}
  where
    nextTimer = previewTimer game - dt

handleEvent :: Event -> Game -> Game
handleEvent event game =
  case event of
    EventKey (Char 'l') Down _ _ -> game {showHandGuide = not (showHandGuide game)}
    EventKey (Char 'L') Down _ _ -> game {showHandGuide = not (showHandGuide game)}
    _ ->
      case phase game of
        PlayingBlind -> handleBlindEvent event game
        BlindCleared -> handleBlindClearedEvent event game
        Shopping -> handleShopEvent event game
        RunWon ->
          case event of
            EventKey (Char 'r') Down _ _ -> rerollRun game
            _ -> game
        RunLost ->
          case event of
            EventKey (Char 'r') Down _ _ -> rerollRun game
            _ -> game

renderGame :: Game -> Picture
renderGame game =
  Pictures
    [ feltBackground
    , drawSidebar game
    , drawJokerRow game
    , drawCenterTable game
    , drawHandArea game
    , drawRightStack game
    , drawMessageBar game
    , drawExpandedJoker game
    , drawSellConfirmation game
    , if phase game == Shopping then drawShopOverlay game else Blank
    , if phase game == BlindCleared then drawBlindClearedOverlay game else Blank
    , if showHandGuide game then drawHandGuideOverlay game else Blank
    , if phase game == RunWon then drawEndOverlay "Run Cleared" "R starts a fresh run." game else Blank
    , if phase game == RunLost then drawEndOverlay "Run Ended" "R starts a fresh run." game else Blank
    ]

handleBlindEvent :: Event -> Game -> Game
handleBlindEvent event game =
  case event of
    EventMotion mousePos ->
      game
        { hoveredSortMode = hoveredSortButton mousePos
        , hoveredCardIndex = hoveredHandCardIndex game mousePos
        }
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      case confirmSellJokerIndex game of
        Just _ -> game
        Nothing -> handleBlindClick mousePos game
    EventKey (Char 's') Down _ _ -> requestSellExpandedJoker game
    EventKey (Char 'S') Down _ _ -> requestSellExpandedJoker game
    EventKey (Char 'c') Down _ _ -> game {confirmSellJokerIndex = Nothing}
    EventKey (Char 'C') Down _ _ -> game {confirmSellJokerIndex = Nothing}
    EventKey (Char key) Down _ _ ->
      case confirmSellJokerIndex game of
        Just _ -> game
        Nothing ->
          case key of
            '1' -> toggleIndex 0 game
            '2' -> toggleIndex 1 game
            '3' -> toggleIndex 2 game
            '4' -> toggleIndex 3 game
            '5' -> toggleIndex 4 game
            '6' -> toggleIndex 5 game
            '7' -> toggleIndex 6 game
            '8' -> toggleIndex 7 game
            'd' -> discardSelected game
            'r' -> rerollRun game
            _ -> game
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      case confirmSellJokerIndex game of
        Just idx -> sellJoker idx game
        Nothing -> playSelectedHand game
    EventKey (SpecialKey KeySpace) Down _ _ ->
      case confirmSellJokerIndex game of
        Just _ -> game
        Nothing -> playSelectedHand game
    _ -> game

handleBlindClearedEvent :: Event -> Game -> Game
handleBlindClearedEvent event game =
  case event of
    EventKey (SpecialKey KeyEnter) Down _ _ -> advanceBlind game
    _ -> game

handleBlindClick :: (Float, Float) -> Game -> Game
handleBlindClick mousePos game =
  case expandedJokerIndex game of
    Just _ ->
      if pointInRect mousePos expandedJokerRect
        then game {expandedJokerIndex = Nothing, confirmSellJokerIndex = Nothing}
        else
          case clickedJokerIndex game mousePos of
            Just idx -> toggleExpandedJoker idx game
            Nothing -> game
    Nothing ->
      case clickedJokerIndex game mousePos of
        Just idx -> toggleExpandedJoker idx game
        Nothing -> handleTableClick mousePos game

handleTableClick :: (Float, Float) -> Game -> Game
handleTableClick mousePos game
  | pointInRect mousePos rankSortButtonRect = sortHandBy compareCardsByRank game
  | pointInRect mousePos suitSortButtonRect = sortHandBy compareCardsBySuit game
  | otherwise =
      case clickedHandIndex game mousePos of
        Just idx -> toggleIndex idx game
        Nothing -> game

toggleExpandedJoker :: Int -> Game -> Game
toggleExpandedJoker idx game
  | expandedJokerIndex game == Just idx = game {expandedJokerIndex = Nothing, confirmSellJokerIndex = Nothing}
  | otherwise = game {expandedJokerIndex = Just idx, confirmSellJokerIndex = Nothing}

requestSellExpandedJoker :: Game -> Game
requestSellExpandedJoker game =
  case expandedJokerIndex game >>= (`safeIndex` jokers game) of
    Just joker ->
      game
        { confirmSellJokerIndex = expandedJokerIndex game
        , message = "Sell " ++ jokerName joker ++ " for $" ++ show (jokerSellValue joker) ++ "? Enter confirms, C cancels."
        }
    Nothing -> game

sellJoker :: Int -> Game -> Game
sellJoker idx game =
  case safeIndex idx (jokers game) of
    Just joker ->
      game
        { money = money game + jokerSellValue joker
        , jokers = removeAt idx (jokers game)
        , expandedJokerIndex = Nothing
        , confirmSellJokerIndex = Nothing
        , message = "Sold " ++ jokerName joker ++ " for $" ++ show (jokerSellValue joker) ++ "."
        }
    Nothing -> game {confirmSellJokerIndex = Nothing}

handleShopEvent :: Event -> Game -> Game
handleShopEvent event game =
  case event of
    EventKey (Char '1') Down _ _ -> buyShopItem 0 game
    EventKey (Char '2') Down _ _ -> buyShopItem 1 game
    EventKey (Char '3') Down _ _ -> buyShopItem 2 game
    EventKey (Char '4') Down _ _ -> buyShopItem 3 game
    EventKey (Char '5') Down _ _ -> buyShopItem 4 game
    EventKey (SpecialKey KeyEnter) Down _ _ -> leaveShop game
    EventKey (Char 'r') Down _ _ -> rerollRun game
    _ -> game

toggleIndex :: Int -> Game -> Game
toggleIndex idx game
  | idx >= length (handCards game) = game
  | idx `elem` selectedIndices game =
      game {selectedIndices = filter (/= idx) (selectedIndices game)}
  | length (selectedIndices game) >= 5 =
      game {message = "You can only select up to 5 cards."}
  | otherwise =
      game {selectedIndices = sort (idx : selectedIndices game)}

discardSelected :: Game -> Game
discardSelected game
  | discardsLeft game <= 0 = game {message = "No discards left on this blind."}
  | null (selectedIndices game) = game {message = "Select cards to discard first."}
  | otherwise =
      let (kept, tossed) = partitionByIndices (selectedIndices game) (handCards game)
       in case refillHandAfterRemoval kept tossed (drawPile game) (discardPile game) of
            Just (refilled, newDrawPile, newDiscard) ->
              game
                { handCards = refilled
                , drawPile = newDrawPile
                , discardPile = newDiscard
                , selectedIndices = []
                , discardsLeft = discardsLeft game - 1
                , playedPreview = []
                , scoredPreview = []
                , previewTimer = 0
                , lastHandScore = 0
                , hoveredCardIndex = Nothing
                , lastPlayedLabel = "Discarded " ++ show (length tossed) ++ " card(s)"
                , message = "Discard used. Build a stronger hand."
                }
            Nothing ->
              game
                { handCards = kept
                , drawPile = []
                , discardPile = discardPile game ++ tossed
                , selectedIndices = []
                , playedPreview = []
                , scoredPreview = []
                , previewTimer = 0
                , hoveredCardIndex = Nothing
                , phase = RunLost
                , lastPlayedLabel = "Deck exhausted"
                , message = "You ran out of cards to draw. Press R for a fresh run."
                }

playSelectedHand :: Game -> Game
playSelectedHand game
  | null (selectedIndices game) = game {message = "Select at least one card to play."}
  | handsLeft game <= 0 = game {message = "No hands left on this blind."}
  | otherwise =
      let selectedCards = map (handCards game !!) (selectedIndices game)
          score = scoreCards (jokers game) (handUpgrades game) (permanentDeck game) selectedCards
          handTotalScore = scoreValue score
          moneyGain = scoreMoney score
          newScore = scoreThisBlind game + handTotalScore
          blind = blinds !! currentBlindIndex game
          wonBlind = newScore >= blindTarget blind
          blindBonusCash = blindPurseIncome (jokers game)
          jokerGoldCash = jokerGoldBlindIncome (jokers game)
          (remainingHand, playedAway) = partitionByIndices (selectedIndices game) (handCards game)
       in case refillHandAfterRemoval remainingHand playedAway (drawPile game) (discardPile game) of
            Just (refilled, newDrawPile, newDiscard) ->
              let updatedGame =
                    game
                      { handCards = refilled
                      , drawPile = newDrawPile
                      , discardPile = newDiscard
                      , selectedIndices = []
                      , handsLeft = handsLeft game - 1
                      , scoreThisBlind = newScore
                      , lastHandScore = handTotalScore
                      , money = money game + moneyGain
                      , playedPreview = selectedCards
                      , scoredPreview = scoreCardsUsed score
                      , previewTimer = 2.2
                      , hoveredCardIndex = Nothing
                      , lastPlayedLabel = describeScoredHand score
                      , message =
                          describeScoredHand score
                            ++ " scored "
                            ++ show handTotalScore
                            ++ pointsSuffix moneyGain
                      }
               in if wonBlind
                    then
                      let interestCash = interestIncome (money updatedGame)
                          payoutCash = blindReward blind + blindBonusCash + jokerGoldCash + interestCash
                       in
                      updatedGame
                        { money = money updatedGame + payoutCash
                        , phase = BlindCleared
                        , message =
                            "Blind cleared with "
                              ++ show newScore
                              ++ "/"
                              ++ show (blindTarget blind)
                              ++ "! Earned $"
                              ++ show (payoutCash + moneyGain)
                              ++ jokerGoldSummary jokerGoldCash
                              ++ interestSummary interestCash
                              ++ "."
                        }
                    else
                      if handsLeft updatedGame <= 0
                        then updatedGame
                              { phase = RunLost
                              , message = "You missed the blind target. Press R for a fresh run."
                              }
                        else updatedGame
            Nothing ->
              let exhaustedGame =
                    game
                      { handCards = remainingHand
                      , drawPile = []
                      , discardPile = discardPile game ++ playedAway
                      , selectedIndices = []
                      , handsLeft = handsLeft game - 1
                      , scoreThisBlind = newScore
                      , lastHandScore = handTotalScore
                      , money = money game + moneyGain
                      , playedPreview = selectedCards
                      , scoredPreview = scoreCardsUsed score
                      , previewTimer = 2.2
                      , hoveredCardIndex = Nothing
                      , lastPlayedLabel = describeScoredHand score
                      }
               in if wonBlind
                    then
                      let interestCash = interestIncome (money exhaustedGame)
                          payoutCash = blindReward blind + blindBonusCash + jokerGoldCash + interestCash
                       in
                      exhaustedGame
                        { money = money exhaustedGame + payoutCash
                        , phase = BlindCleared
                        , message =
                            "Blind cleared with "
                              ++ show newScore
                              ++ "/"
                              ++ show (blindTarget blind)
                              ++ "! Earned $"
                              ++ show (payoutCash + moneyGain)
                              ++ jokerGoldSummary jokerGoldCash
                              ++ interestSummary interestCash
                              ++ "."
                        }
                    else
                      exhaustedGame
                        { phase = RunLost
                        , lastPlayedLabel = "Deck exhausted"
                        , message = "You ran out of cards to draw. Press R for a fresh run."
                        }

advanceBlind :: Game -> Game
advanceBlind game =
  let nextIndex = currentBlindIndex game + 1
   in if nextIndex >= length blinds
        then game
              { phase = RunWon
              , currentBlindIndex = length blinds - 1
              , message = "Boss blind defeated. Press R for another run."
              }
        else openShop (game {currentBlindIndex = nextIndex})

openShop :: Game -> Game
openShop game =
  let availableJokers = filter (\joker -> all ((/= jokerId joker) . jokerId) (jokers game)) jokerPool
      (jokerOffers, genJokers) = randomJokerOffers (jokerOfferWeight availableJokers) (generator game)
      (cardOffers, genCards) = randomEnhancedCardOffers 5 genJokers
      upgradeOffers = map (`ShopUpgrade` 5) upgradeHandPool
      offerPool = jokerOffers ++ cardOffers ++ upgradeOffers
      (shuffledOffers, gen1) = shuffleList offerPool genCards
   in game
        { phase = Shopping
        , shopItems = take 5 shuffledOffers
        , removalsLeft = 0
        , shopDeckCursor = 0
        , message = "Shop: buy with 1-5, Enter starts the next blind."
        , generator = gen1
        }

leaveShop :: Game -> Game
leaveShop game =
  startBlind $
    game
      { phase = PlayingBlind
      , shopItems = []
      , message = "New blind started."
      }

buyShopItem :: Int -> Game -> Game
buyShopItem idx game
  | idx >= length (shopItems game) = game
  | otherwise =
      case shopItems game !! idx of
        ShopJoker joker
          | money game < jokerCost joker ->
              game {message = "Not enough cash for that joker."}
          | otherwise ->
              game
                { money = money game - jokerCost joker
                , jokers = jokers game ++ [joker]
                , shopItems = removeAt idx (shopItems game)
                , message = jokerName joker ++ " added to your joker tray."
                }
        ShopCard card price
          | money game < price ->
              game {message = "Not enough cash for that card."}
          | otherwise ->
              game
                { money = money game - price
                , permanentDeck = permanentDeck game ++ [card]
                , shopItems = removeAt idx (shopItems game)
                , message = showPlayingCard card ++ " joined your deck."
                }
        ShopUpgrade handKind price
          | money game < price ->
              game {message = "Not enough cash for that upgrade."}
          | otherwise ->
              let nextLevel = handUpgradeLevel handKind game + 1
               in game
                    { money = money game - price
                    , handUpgrades = upgradeHand handKind (handUpgrades game)
                    , shopItems = removeAt idx (shopItems game)
                    , message = handTypeName handKind ++ " upgraded to Lv " ++ show nextLevel ++ "."
                    }

moveShopCursor :: Int -> Game -> Game
moveShopCursor delta game
  | null (permanentDeck game) = game
  | otherwise =
      let cap = length (permanentDeck game) - 1
          nextCursor = max 0 (min cap (shopDeckCursor game + delta))
       in game {shopDeckCursor = nextCursor}

removeDeckCard :: Game -> Game
removeDeckCard game
  | removalsLeft game <= 0 = game {message = "You already removed 3 cards in this shop."}
  | null (permanentDeck game) = game
  | length (permanentDeck game) <= 5 = game {message = "Deck is already lean enough."}
  | otherwise =
      let idx = shopDeckCursor game
          removedCard = permanentDeck game !! idx
          nextDeck = removeAt idx (permanentDeck game)
          nextCursor = max 0 (min (length nextDeck - 1) idx)
       in game
            { permanentDeck = nextDeck
            , removalsLeft = removalsLeft game - 1
            , shopDeckCursor = nextCursor
            , message = "Removed " ++ showPlayingCard removedCard ++ " from your deck."
            }

startBlind :: Game -> Game
startBlind game =
  let (shuffledDeck, gen1) = shuffleList (permanentDeck game) (generator game)
      (openingHand, rest) = drawUpTo 8 shuffledDeck []
      blind = blinds !! currentBlindIndex game
   in game
        { drawPile = rest
        , handCards = openingHand
        , discardPile = []
        , playedPreview = []
        , scoredPreview = []
        , previewTimer = 0
        , selectedIndices = []
        , scoreThisBlind = 0
        , lastHandScore = 0
        , handsLeft = 4 + extraHandCount (jokers game)
        , discardsLeft = 3 + extraDiscardCount (jokers game)
        , phase = PlayingBlind
        , shopItems = []
        , removalsLeft = 0
        , shopDeckCursor = 0
        , hoveredSortMode = Nothing
        , hoveredCardIndex = Nothing
        , expandedJokerIndex = Nothing
        , confirmSellJokerIndex = Nothing
        , lastPlayedLabel = "Play a hand"
        , message = "Ante " ++ show (blindAnte blind) ++ " " ++ blindLabel (blindKind blind) ++ ": beat " ++ show (blindTarget blind) ++ "."
        , generator = gen1
        }

rerollRun :: Game -> Game
rerollRun game =
  let (seed, _) = randomR (1, 1000000) (generator game)
   in startFreshGame seed

scoreCards :: [Joker] -> [(HandType, Int)] -> [PlayingCard] -> [PlayingCard] -> ScoreBreakdown
scoreCards activeJokers upgrades deck selected =
  let (base, scoringCards) = evaluateBestHand activeJokers selected
      upgradedBase = applyHandUpgrade upgrades base
      rankChips = sum (map cardChipValue scoringCards)
      enchantChips = sum [20 | card <- scoringCards, cardEnchant card == BonusChips]
      enchantMult = sum [2 | card <- scoringCards, cardEnchant card == LuckyMult]
      timesMultCount = length [card | card <- scoringCards, cardEnchant card == TimesMult]
      jokerEnchantChips = sum [20 | joker <- activeJokers, jokerEnchant joker == BonusChips]
      jokerEnchantMult = sum [2 | joker <- activeJokers, jokerEnchant joker == LuckyMult]
      jokerTimesMultCount = length [joker | joker <- activeJokers, jokerEnchant joker == TimesMult]
      enchantMultFactor = 1.5 ^^ (timesMultCount + jokerTimesMultCount)
      (jokerXMultFactor, jokerXMultNotes) = jokerXMult activeJokers deck
      goldMoney = sum [3 | card <- scoringCards, cardEnchant card == GoldSeal]
      (jokerChipBonus, jokerMultBonus, jokerNotes) = applyJokers activeJokers scoringCards (handType upgradedBase)
   in ScoreBreakdown
        { scoreBase = upgradedBase
        , scoreCardsUsed = scoringCards
        , scoreChipBonus = rankChips + enchantChips + jokerEnchantChips + jokerChipBonus
        , scoreMultBonus = enchantMult + jokerEnchantMult + jokerMultBonus
        , scoreMultFactor = enchantMultFactor * jokerXMultFactor
        , scoreMoneyGain = goldMoney
        , scoreNotes =
            jokerNotes
              ++ jokerEnchantNotes activeJokers
              ++ xMultNotes (timesMultCount + jokerTimesMultCount) enchantMultFactor
              ++ jokerXMultNotes
              ++ moneyNotes goldMoney
        }

scoreValue :: ScoreBreakdown -> Int
scoreValue breakdown =
  (baseChips (scoreBase breakdown) + scoreChipBonus breakdown)
    * scoreTotalMult breakdown

scoreMoney :: ScoreBreakdown -> Int
scoreMoney = scoreMoneyGain

describeScoredHand :: ScoreBreakdown -> String
describeScoredHand breakdown =
  handTypeName (handType (scoreBase breakdown))
    ++ " | "
    ++ show (baseChips (scoreBase breakdown) + scoreChipBonus breakdown)
    ++ " x "
    ++ show (scoreTotalMult breakdown)
    ++ bonusSummary (scoreNotes breakdown)

scoreTotalMult :: ScoreBreakdown -> Int
scoreTotalMult breakdown =
  floor $
    fromIntegral (baseMult (scoreBase breakdown) + scoreMultBonus breakdown)
      * scoreMultFactor breakdown

bonusSummary :: [String] -> String
bonusSummary [] = ""
bonusSummary notes = " | " ++ intercalate ", " notes

moneyNotes :: Int -> [String]
moneyNotes 0 = []
moneyNotes value = ["Gold card paid $" ++ show value]

jokerEnchantNotes :: [Joker] -> [String]
jokerEnchantNotes activeJokers =
  chipNote ++ multNote
  where
    chipCount = length [joker | joker <- activeJokers, jokerEnchant joker == BonusChips]
    multCount = length [joker | joker <- activeJokers, jokerEnchant joker == LuckyMult]
    chipNote =
      if chipCount > 0
        then ["Enhanced joker +" ++ show (chipCount * 20) ++ " chips"]
        else []
    multNote =
      if multCount > 0
        then ["Enhanced joker +" ++ show (multCount * 2) ++ " mult"]
        else []

xMultNotes :: Int -> Float -> [String]
xMultNotes 0 _ = []
xMultNotes count factor =
  ["X mult " ++ showXMult factor ++ " from " ++ show count ++ " source(s)"]

xMultSummary :: Float -> String
xMultSummary factor
  | factor == 1 = ""
  | otherwise = " x" ++ showXMult factor

showXMult :: Float -> String
showXMult factor =
  let rounded = fromIntegral (round (factor * 100)) / 100 :: Float
   in show rounded

jokerXMult :: [Joker] -> [PlayingCard] -> (Float, [String])
jokerXMult activeJokers deck =
  (product factors, notes)
  where
    jokerCount = length activeJokers
    emptySlotFactor = fromIntegral (max 0 (5 - jokerCount)) :: Float
    enhancedCount = length [card | card <- deck, cardEnchant card /= NoEnchant]
    enhancedFactor = 1 + fromIntegral enhancedCount * 0.25
    hasEmptyStage = any ((== EmptyStage) . jokerId) activeJokers
    hasEnhancementEngine = any ((== EnhancementEngine) . jokerId) activeJokers
    factors =
      (if hasEmptyStage then [emptySlotFactor] else [])
        ++ (if hasEnhancementEngine then [enhancedFactor] else [])
    notes =
      (if hasEmptyStage then ["Empty Stage x" ++ showXMult emptySlotFactor ++ " mult"] else [])
        ++ (if hasEnhancementEngine then ["Enhancement Engine x" ++ showXMult enhancedFactor ++ " mult"] else [])

applyJokers :: [Joker] -> [PlayingCard] -> HandType -> (Int, Int, [String])
applyJokers activeJokers selected scoredHandType =
  foldr addJoker (0, 0, []) activeJokers
  where
    faceCount = length (filter (isFace . cardRank) selected)
    aceCount = length (filter ((== Ace) . cardRank) selected)
    redCount = length (filter (\card -> cardSuit card == Hearts || cardSuit card == Diamonds || cardEnchant card == WildSuit) selected)
    groupedRanks = group (sort (map cardRank selected))
    hasPair = any (\sameRanks -> length sameRanks >= 2) groupedRanks
    addJoker joker (chipAcc, multAcc, notesAcc) =
      case jokerId joker of
        FlatMultJoker -> (chipAcc, multAcc + 5, notesAcc ++ ["+5 mult"])
        PairPrinter ->
          if hasPair
            then (chipAcc + 40, multAcc, notesAcc ++ ["Pair Printer +40 chips"])
            else (chipAcc, multAcc, notesAcc)
        FaceCollector ->
          if faceCount > 0
            then (chipAcc, multAcc + faceCount * 2, notesAcc ++ ["Face Collector +" ++ show (faceCount * 2) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        GreedyJoker ->
          if redCount > 0
            then (chipAcc, multAcc + redCount * 2, notesAcc ++ ["Greedy Joker + " ++ show (redCount * 2) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        StraightShooter ->
          if scoredHandType == Straight || scoredHandType == StraightFlush || scoredHandType == RoyalFlush
            then (chipAcc + 80, multAcc, notesAcc ++ ["Straight Shooter +80 chips"])
            else (chipAcc, multAcc, notesAcc)
        BlindPurse -> (chipAcc, multAcc, notesAcc)
        ChipSprinter -> (chipAcc + 25, multAcc, notesAcc ++ ["Chip Sprinter +25 chips"])
        TwinPair ->
          if scoredHandType == TwoPair
            then (chipAcc, multAcc + 6, notesAcc ++ ["Twin Pair +6 mult"])
            else (chipAcc, multAcc, notesAcc)
        AcePilot ->
          if aceCount > 0
            then (chipAcc, multAcc + aceCount * 4, notesAcc ++ ["Ace Pilot +" ++ show (aceCount * 4) ++ " mult"])
            else (chipAcc, multAcc, notesAcc)
        ExtraDiscard -> (chipAcc, multAcc, notesAcc)
        ExtraHand -> (chipAcc, multAcc, notesAcc)
        EmptyStage -> (chipAcc, multAcc, notesAcc)
        SuitPainter -> (chipAcc, multAcc, notesAcc)
        ShortcutArtist -> (chipAcc, multAcc, notesAcc)
        EnhancementEngine -> (chipAcc, multAcc, notesAcc)

evaluateHand :: [Joker] -> [PlayingCard] -> HandScore
evaluateHand activeJokers cards =
  let ranks = sort (map cardRank cards)
      suits = map (effectiveSuitWith activeJokers) cards
      grouped = sortOn (negate . length) (group ranks)
      counts = sortOn negate (map length grouped)
      hasShortcut = any ((== ShortcutArtist) . jokerId) activeJokers
      isFlushHand = (length cards == 5 || hasShortcut && length cards == 4) && allSame suits
      isStraightHand = (length cards == 5 || hasShortcut && length cards == 4) && isStraight ranks
      isRoyalHand = ranks == [Ten, Jack, Queen, King, Ace]
   in case () of
        _
          | counts == [5] && isFlushHand -> HandScore FlushFive 180 16
          | counts == [3, 2] && isFlushHand -> HandScore FlushHouse 160 14
          | isRoyalHand && isFlushHand -> HandScore RoyalFlush 120 10
          | isStraightHand && isFlushHand -> HandScore StraightFlush 100 8
          | counts == [5] -> HandScore FiveKind 140 12
          | counts == [4] || counts == [4, 1] -> HandScore FourKind 60 7
          | counts == [3, 2] -> HandScore FullHouse 45 4
          | isFlushHand -> HandScore Flush 35 4
          | isStraightHand -> HandScore Straight 40 4
          | counts == [3] || counts == [3, 1] || counts == [3, 1, 1] -> HandScore ThreeKind 35 3
          | counts == [2, 2] || counts == [2, 2, 1] -> HandScore TwoPair 25 2
          | counts == [2] || counts == [2, 1] || counts == [2, 1, 1] || counts == [2, 1, 1, 1] -> HandScore OnePair 15 2
          | otherwise -> HandScore HighCard 5 1

evaluateBestHand :: [Joker] -> [PlayingCard] -> (HandScore, [PlayingCard])
evaluateBestHand _ [] = (HandScore HighCard 5 1, [])
evaluateBestHand activeJokers cards =
  maximumByHand
    [ (evaluateHand activeJokers subset, subset)
    | subset <- nonEmptySubsets cards
    , length subset <= 5
    ]

maximumByHand :: [(HandScore, [PlayingCard])] -> (HandScore, [PlayingCard])
maximumByHand [] = (HandScore HighCard 5 1, [])
maximumByHand (firstResult : restResults) =
  foldl chooseBetter firstResult restResults
  where
    chooseBetter best current =
      if compareHandResult current best == GT
        then current
        else best

compareHandResult :: (HandScore, [PlayingCard]) -> (HandScore, [PlayingCard]) -> Ordering
compareHandResult (scoreA, cardsA) (scoreB, cardsB) =
  compare (handRankValue (handType scoreA), negate (length cardsA), subsetStrength cardsA)
          (handRankValue (handType scoreB), negate (length cardsB), subsetStrength cardsB)

handRankValue :: HandType -> Int
handRankValue handKind =
  case handKind of
    HighCard -> 0
    OnePair -> 1
    TwoPair -> 2
    ThreeKind -> 3
    Straight -> 4
    Flush -> 5
    FullHouse -> 6
    FourKind -> 7
    StraightFlush -> 8
    RoyalFlush -> 9
    FiveKind -> 10
    FlushHouse -> 11
    FlushFive -> 12

subsetStrength :: [PlayingCard] -> Int
subsetStrength = sum . map cardChipValue

nonEmptySubsets :: [a] -> [[a]]
nonEmptySubsets [] = []
nonEmptySubsets (x : xs) =
  [x] : map (x :) rest ++ rest
  where
    rest = nonEmptySubsets xs

drawSidebar :: Game -> Picture
drawSidebar game =
  let blind = blinds !! currentBlindIndex game
      blindName = blindLabel (blindKind blind)
      selectedLabel =
        if null (selectedIndices game)
          then "No cards selected"
          else show (length (selectedIndices game)) ++ " card(s) selected"
   in Pictures
        [ Color (makeColorI 47 54 34 255) (Polygon [(-640, -360), (-350, -360), (-350, 360), (-640, 360)])
        , translate (-610) 280 $ drawPanelTitle blindName
        , translate (-610) 172 $ drawReadout "Target" (show (blindTarget blind))
        , translate (-610) 92 $ drawReadout "Round score" (show (scoreThisBlind game))
        , translate (-610) 12 $ drawHandBadge game
        , translate (-610) (-118) $ drawRunBlock game
        , translate (-610) (-292) $ scale 0.11 0.11 $ color white $ Text selectedLabel
        ]

drawJokerRow :: Game -> Picture
drawJokerRow game =
  Pictures $
    zipWith drawSlot [0 ..] [0 .. 4]
  where
    owned = jokers game
    drawSlot idx offset =
      let x = -110 + fromIntegral offset * 152
       in translate x 250 $
            case safeIndex idx owned of
              Just joker -> drawJokerCard True joker
              Nothing -> drawEmptyJokerSlot

drawExpandedJoker :: Game -> Picture
drawExpandedJoker game =
  case expandedJokerIndex game >>= (`safeIndex` jokers game) of
    Just joker ->
      Pictures
        [ Color (makeColor 0 0 0 0.34) (rectangleSolid 1280 720)
        , translate 120 30 $ scale 2.45 2.45 $ drawJokerCard False joker
        , translate (-18) (-190) $ scale 0.13 0.13 $ color (greyN 0.92) $
            Text "Press S to sell"
        ]
    Nothing -> Blank

drawSellConfirmation :: Game -> Picture
drawSellConfirmation game =
  case confirmSellJokerIndex game >>= (`safeIndex` jokers game) of
    Just joker ->
      Pictures
        [ Color (makeColor 0 0 0 0.46) (rectangleSolid 1280 720)
        , Color (makeColorI 235 182 47 255) (rectangleSolid 520 156)
        , Color (makeColorI 40 48 54 255) (translate 0 (-6) (rectangleSolid 492 128))
        , translate (-210) 32 $ scale 0.24 0.24 $ color white $ Text "Sell Joker?"
        , translate (-210) (-8) $ drawWrappedLabel 42 17 0.13 (greyN 0.92) $
            jokerName joker ++ " sells for $" ++ show (jokerSellValue joker) ++ "."
        , translate (-210) (-54) $ scale 0.13 0.13 $ color (light orange) $
            Text "Enter confirms, C cancels"
        ]
    Nothing -> Blank

drawCenterTable :: Game -> Picture
drawCenterTable game =
  Pictures
    [ translate (-420) 180 $
        scale 0.24 0.24 $
          color white $
            Text ("+" ++ show (lastHandScore game))
    , translate (-420) 140 $ drawWrappedLabel 48 25 0.18 (greyN 0.95) (lastPlayedLabel game)
    , Color (makeColor 1 1 1 (previewOpacity game)) $
        Pictures (zipWith (drawPlayedCard (scoredPreview game)) [0 ..] (playedPreview game))
    ]

drawHandArea :: Game -> Picture
drawHandArea game =
  Pictures
    [ Color (makeColor 0 0 0 0.18) (translate 120 (-250) (rectangleSolid 760 160))
    , drawSortButtons game
    , Pictures (zipWith (drawHandCard game) [0 ..] (handCards game))
    ]

drawRightStack :: Game -> Picture
drawRightStack game =
  Pictures
    [ translate 555 (-110) $ drawDeckBox "Deck" (show (length (drawPile game)))
    , translate 555 55 $ drawDeckBox "Discard" (show (length (discardPile game)))
    , translate 470 55 $ drawDiscardPreview game
    ]

drawMessageBar :: Game -> Picture
drawMessageBar game =
  translate (-330) (-336) $
    drawWrappedLabel 92 14 0.11 white (message game ++ " | L hand guide")

drawShopOverlay :: Game -> Picture
drawShopOverlay game =
  Pictures
    [ Color (makeColor 0 0 0 0.7) (rectangleSolid 1280 720)
    , Color (makeColorI 235 182 47 255) (translate 0 56 (rectangleSolid 900 360))
    , Color (makeColorI 40 48 54 255) (translate 0 48 (rectangleSolid 860 320))
    , translate (-390) 176 $ scale 0.3 0.3 $ color white $ Text "Shop"
    , translate (-390) 138 $ scale 0.11 0.11 $ color (greyN 0.92) $
        Text "Buy with 1-5. Enter starts the next blind."
    , Pictures (zipWith drawShopCard [0 ..] (shopItems game))
    , translate 250 (138) $ scale 0.18 0.18 $ color (light orange) $
        Text ("Cash $" ++ show (money game))
    ]

drawBlindClearedOverlay :: Game -> Picture
drawBlindClearedOverlay game =
  Pictures
    [ Color (makeColor 0 0 0 0.32) (rectangleSolid 1280 720)
    , Color (makeColorI 235 182 47 255) (rectangleSolid 520 170)
    , Color (makeColorI 40 48 54 255) (translate 0 (-6) (rectangleSolid 492 142))
    , translate (-210) 36 $ scale 0.28 0.28 $ color white $ Text "Blind Passed"
    , translate (-210) (-8) $ drawWrappedLabel 42 17 0.13 (greyN 0.92) (message game)
    , translate (-210) (-58) $ scale 0.14 0.14 $ color (light orange) $
        Text "Press Enter to move on"
    ]

drawEndOverlay :: String -> String -> Game -> Picture
drawEndOverlay title subtitle game =
  Pictures
    [ Color (makeColor 0 0 0 0.72) (rectangleSolid 1280 720)
    , Color (makeColorI 39 48 54 255) (rectangleSolid 720 220)
    , translate (-230) 28 $ scale 0.42 0.42 $ color white $ Text title
    , translate (-230) (-30) $ scale 0.15 0.15 $ color (greyN 0.9) $ Text subtitle
    , translate (-230) (-78) $ scale 0.12 0.12 $ color (light orange) $
        Text ("Final bankroll $" ++ show (money game))
    ]

drawShopCard :: Int -> ShopItem -> Picture
drawShopCard idx item =
  let x = -320 + fromIntegral idx * 160
   in translate x (-6) $
        case item of
          ShopJoker joker ->
            Pictures
              [ drawStoreFrame (show (idx + 1))
              , translate 0 (-6) $ scale 0.62 0.62 (drawJokerCard False joker)
              , translate (-58) (-98) $ scale 0.12 0.12 $ color white $
                  Text ("$" ++ show (jokerCost joker))
              ]
          ShopCard card price ->
            Pictures
              [ drawStoreFrame (show (idx + 1))
              , translate 0 (-4) $ scale 1.02 1.02 (cardFacePicture 1 False False card)
              , translate (-58) (-98) $ scale 0.12 0.12 $ color white $
                  Text ("$" ++ show price)
              ]
          ShopUpgrade handKind price ->
            Pictures
              [ drawStoreFrame (show (idx + 1))
              , translate (-54) 42 $ scale 0.11 0.11 $ color (makeColorI 235 182 47 255) $ Text "UPGRADE"
              , translate (-54) 4 $ drawWrappedLabel 13 13 0.09 white (handTypeName handKind)
              , translate (-54) (-52) $ scale 0.08 0.08 $ color (greyN 0.9) $ Text "+10 chips, +1 mult"
              , translate (-58) (-98) $ scale 0.12 0.12 $ color white $
                  Text ("$" ++ show price)
              ]

drawStoreFrame :: String -> Picture
drawStoreFrame hotkey =
  Pictures
    [ Color (makeColorI 56 67 74 255) (rectangleSolid 142 228)
    , Color (makeColorI 240 240 240 255) (rectangleWire 142 228)
    , translate (-54) 88 $ scale 0.16 0.16 $ color white $ Text hotkey
    ]

drawDeckBox :: String -> String -> Picture
drawDeckBox label value =
  Pictures
    [ Color (makeColorI 230 231 240 255) (rectangleSolid 108 132)
    , Color (makeColorI 215 86 74 255) (rectangleWire 108 132)
    , translate (-38) 36 $ scale 0.12 0.12 $ color black $ Text label
    , translate (-28) (-14) $ scale 0.18 0.18 $ color black $ Text value
    ]

drawDiscardPreview :: Game -> Picture
drawDiscardPreview game =
  Pictures $
    zipWith drawPreviewCard [0 ..] (take 5 (reverse (discardPile game)))
  where
    drawPreviewCard idx card =
      rotate (-16 + fromIntegral idx * 8) $
        translate (fromIntegral idx * 14) (fromIntegral idx * (-4)) $
          cardFacePicture 0.78 False False card

drawPlayedCard :: [PlayingCard] -> Int -> PlayingCard -> Picture
drawPlayedCard scoredCards idx card =
  translate (-180 + fromIntegral idx * 120) 20 $
    cardFacePicture 1.95 (card `elem` scoredCards) False card

drawHandCard :: Game -> Int -> PlayingCard -> Picture
drawHandCard game idx card =
  let selected = idx `elem` selectedIndices game
      hovered = hoveredCardIndex game == Just idx
      x = -180 + fromIntegral idx * 88
      y
        | selected = -185
        | hovered = -198
        | otherwise = -215
   in Pictures
        [ translate x y (cardFacePicture 2.0 selected hovered card)
        , translate (x - 8) (y + 92) $ scale 0.14 0.14 $ color (makeColorI 255 236 166 255) $
            Text (show (idx + 1))
        ]

cardFacePicture :: Float -> Bool -> Bool -> PlayingCard -> Picture
cardFacePicture scaleFactor selected hovered card =
  scale scaleFactor scaleFactor $
    Pictures
      [ if hovered && not selected
          then Color (makeColorI 209 230 255 255) (rectangleSolid 60 82)
          else Blank
      , if selected
          then Color (makeColorI 255 246 169 255) (rectangleSolid 54 76)
          else
            if hovered
              then Color (makeColorI 248 251 255 255) (rectangleSolid 54 76)
              else Color white (rectangleSolid 54 76)
      , Color (makeColorI 40 48 54 255) (rectangleWire 54 76)
      , translate (-17) 16 $ scale 0.12 0.12 $ color ink $ Text (rankLabel (cardRank card))
      , translate (-17) (-2) $ scale 0.12 0.12 $ color ink $ Text (suitGlyph (cardSuit card))
      , translate (-18) (-24) $ scale 0.07 0.07 $ color enchantColor $
          Text (enchantShort (cardEnchant card))
      ]
  where
    ink =
      case cardSuit card of
        Hearts -> makeColorI 202 64 66 255
        Diamonds -> makeColorI 202 64 66 255
        _ -> makeColorI 38 58 84 255
    enchantColor =
      case cardEnchant card of
        NoEnchant -> makeColorI 140 140 140 255
        BonusChips -> makeColorI 50 157 255 255
        LuckyMult -> makeColorI 255 109 83 255
        TimesMult -> makeColorI 179 112 255 255
        GoldSeal -> makeColorI 244 181 54 255
        WildSuit -> makeColorI 111 214 170 255

drawSortButtons :: Game -> Picture
drawSortButtons game =
  Pictures
    [ translate (-360) (-200) $ drawSortButton "Rank" (hoveredSortMode game == Just SortByRank)
    , translate (-360) (-260) $ drawSortButton "Suit" (hoveredSortMode game == Just SortBySuit)
    ]

drawSortButton :: String -> Bool -> Picture
drawSortButton label isHovered =
  Pictures
    [ if isHovered
        then Color (makeColorI 236 188 72 255) (rectangleSolid 98 46)
        else Color (makeColorI 42 55 61 255) (rectangleSolid 90 40)
    , if isHovered
        then Color (makeColorI 255 245 210 255) (rectangleWire 98 46)
        else Color (makeColorI 228 231 236 255) (rectangleWire 90 40)
    , translate (if isHovered then (-30) else (-28)) (-7) $
        scale 0.12 0.12 $
          color white $
            Text label
    ]

drawWrappedLabel :: Int -> Float -> Float -> Color -> String -> Picture
drawWrappedLabel maxChars lineStep textScale textColor content =
  Pictures $
    zipWith
      (\idx lineText ->
          translate 0 (negate (fromIntegral idx * lineStep)) $
            scale textScale textScale $
              color textColor $
                Text lineText
      )
      [0 :: Int ..]
      (wrapWords maxChars content)

drawJokerCard :: Bool -> Joker -> Picture
drawJokerCard compact joker =
  let scaler = if compact then 0.92 else 1.0
   in scale scaler scaler $
        Pictures
          [ Color (makeColorI 245 245 245 255) (rectangleSolid 116 156)
          , Color (makeColorI 55 67 74 255) (rectangleWire 116 156)
          , translate (-48) 56 $ scale 0.12 0.12 $ color (makeColorI 48 64 79 255) $ Text "JOKER"
          , translate (-48) 14 $ drawWrappedLabel 12 14 0.11 (makeColorI 226 84 84 255) (jokerName joker)
          , translate (-48) (-28) $ drawWrappedLabel 18 14 0.07 (makeColorI 48 64 79 255) (jokerText joker)
          , translate (-48) (-62) $ scale 0.08 0.08 $ color (enchantDisplayColor (jokerEnchant joker)) $
              Text (jokerEnchantLabel (jokerEnchant joker))
          ]

drawEmptyJokerSlot :: Picture
drawEmptyJokerSlot =
  Pictures
    [ Color (makeColorI 57 83 74 255) (rectangleSolid 116 156)
    , Color (makeColorI 195 214 209 255) (rectangleWire 116 156)
    , translate (-36) (-6) $ scale 0.14 0.14 $ color (greyN 0.9) $ Text "EMPTY"
    ]

drawPanelTitle :: String -> Picture
drawPanelTitle title =
  Pictures
    [ Color (makeColorI 182 120 15 255) (rectangleSolid 230 58)
    , translate (-95) (-10) $ scale 0.28 0.28 $ color white $ Text title
    ]

drawReadout :: String -> String -> Picture
drawReadout label value =
  Pictures
    [ Color (makeColorI 39 48 54 255) (rectangleSolid 230 58)
    , translate (-100) 8 $ scale 0.11 0.11 $ color (greyN 0.9) $ Text label
    , translate (-100) (-16) $ scale 0.18 0.18 $ color white $ Text value
    ]

drawHandBadge :: Game -> Picture
drawHandBadge game =
  Pictures
    [ Color (makeColorI 38 48 54 255) (rectangleSolid 230 98)
    , translate (-100) 18 $ scale 0.11 0.11 $ color (greyN 0.9) $ Text "Current hand"
    , translate (-100) (-18) $ scale 0.24 0.24 $ color white $
        Text (handTypeName (handType (fst (evaluateBestHand (jokers game) selected))))
    ]
  where
    selected = map (handCards game !!) (selectedIndices game)

drawHandGuideOverlay :: Game -> Picture
drawHandGuideOverlay game =
  Pictures
    [ Color (makeColor 0 0 0 0.74) (rectangleSolid 1280 720)
    , Color (makeColorI 235 182 47 255) (translate 0 0 (rectangleSolid 620 560))
    , Color (makeColorI 56 67 74 255) (translate 0 0 (rectangleSolid 590 530))
    , translate (-255) 232 $ scale 0.22 0.22 $ color white $ Text "Poker Hands"
    , translate (210) 232 $ scale 0.1 0.1 $ color (greyN 0.92) $ Text "L closes"
    , Pictures (zipWith (drawGuideRow game) [0 ..] pokerHandGuideRows)
    ]

drawGuideRow :: Game -> Int -> (HandType, Int, Int) -> Picture
drawGuideRow game idx (handKind, chips, multValue) =
  let y = 170 - fromIntegral idx * 34
      level = handUpgradeLevel handKind game
      upgraded = level > 0
      shownChips = chips + level * upgradeChipsPerLevel
      shownMult = multValue + level * upgradeMultPerLevel
      numberColor = if upgraded then makeColorI 255 226 103 255 else white
   in translate 0 y $
        Pictures
          [ Color (makeColorI 196 202 210 255) (translate 0 0 (rectangleSolid 540 28))
          , Color (makeColorI 66 77 85 255) (translate 0 0 (rectangleWire 540 28))
          , translate (-240) (-7) $ scale 0.11 0.11 $ color (makeColorI 48 58 68 255) $ Text (handTypeName handKind)
          , Color (makeColorI 35 138 239 255) (translate 55 0 (rectangleSolid 84 22))
          , Color (makeColorI 252 86 80 255) (translate 124 0 (rectangleSolid 74 22))
          , translate 22 (-7) $ scale 0.11 0.11 $ color numberColor $ Text (show shownChips)
          , translate 102 (-7) $ scale 0.11 0.11 $ color numberColor $ Text ("x" ++ show shownMult)
          , if upgraded
              then translate 175 (-7) $ scale 0.1 0.1 $ color (makeColorI 255 226 103 255) $ Text ("Lv " ++ show level)
              else Blank
          ]

drawRunBlock :: Game -> Picture
drawRunBlock game =
  Pictures
    [ Color (makeColorI 31 38 43 255) (rectangleSolid 230 150)
    , translate (-100) 42 $ scale 0.12 0.12 $ color white $
        Text ("Hands " ++ show (handsLeft game))
    , translate (-100) 8 $ scale 0.12 0.12 $ color white $
        Text ("Discards " ++ show (discardsLeft game))
    , translate (-100) (-26) $ scale 0.12 0.12 $ color white $
        Text ("Money $" ++ show (money game))
    , translate (-100) (-60) $ scale 0.12 0.12 $ color white $
        Text ("Ante " ++ show (blindAnte (blinds !! currentBlindIndex game)) ++ "/" ++ show totalAntes)
    ]

feltBackground :: Picture
feltBackground =
  Pictures
    [ Color (makeColorI 33 115 87 255) (rectangleSolid 1280 720)
    , Color (makeColor 0 0 0 0.05) (ThickCircle 170 450)
    , translate 210 70 $ Color (makeColor 1 1 1 0.03) (ThickCircle 120 220)
    , translate 430 (-120) $ Color (makeColor 0 0 0 0.05) (ThickCircle 80 180)
    ]

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

rankLabel :: Rank -> String
rankLabel rankValue =
  case rankValue of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

suitGlyph :: Suit -> String
suitGlyph suitValue =
  case suitValue of
    Clubs -> "C"
    Diamonds -> "D"
    Hearts -> "H"
    Spades -> "S"

enchantShort :: Enchant -> String
enchantShort enchantment =
  case enchantment of
    NoEnchant -> ""
    BonusChips -> "+20c"
    LuckyMult -> "+2m"
    TimesMult -> "x1.5"
    GoldSeal -> "$3"
    WildSuit -> "wild"

jokerEnchantLabel :: Enchant -> String
jokerEnchantLabel enchantment =
  case enchantment of
    NoEnchant -> ""
    BonusChips -> "+20 chips"
    LuckyMult -> "+2 mult"
    TimesMult -> "x1.5 mult"
    GoldSeal -> "$3/blind"
    WildSuit -> ""

enchantDisplayColor :: Enchant -> Color
enchantDisplayColor enchantment =
  case enchantment of
    NoEnchant -> makeColorI 140 140 140 255
    BonusChips -> makeColorI 50 157 255 255
    LuckyMult -> makeColorI 255 109 83 255
    TimesMult -> makeColorI 179 112 255 255
    GoldSeal -> makeColorI 244 181 54 255
    WildSuit -> makeColorI 111 214 170 255

effectiveSuit :: PlayingCard -> Suit
effectiveSuit card =
  case cardEnchant card of
    WildSuit -> Hearts
    _ -> cardSuit card

effectiveSuitWith :: [Joker] -> PlayingCard -> Suit
effectiveSuitWith activeJokers card =
  let baseSuit = effectiveSuit card
   in if any ((== SuitPainter) . jokerId) activeJokers
        then suitColorGroup baseSuit
        else baseSuit

suitColorGroup :: Suit -> Suit
suitColorGroup suit =
  case suit of
    Hearts -> Hearts
    Diamonds -> Hearts
    Clubs -> Spades
    Spades -> Spades

cardChipValue :: PlayingCard -> Int
cardChipValue card =
  case cardRank card of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11

showPlayingCard :: PlayingCard -> String
showPlayingCard card =
  rankLabel (cardRank card) ++ suitGlyph (cardSuit card) ++ enchantSuffix (cardEnchant card)

enchantSuffix :: Enchant -> String
enchantSuffix enchantment =
  case enchantment of
    NoEnchant -> ""
    BonusChips -> " (+20 chips)"
    LuckyMult -> " (+2 mult)"
    TimesMult -> " (x1.5 mult)"
    GoldSeal -> " (gold)"
    WildSuit -> " (wild)"

drawUpTo :: Int -> [a] -> [a] -> ([a], [a])
drawUpTo target source existing
  | length existing >= target = (existing, source)
drawUpTo _ [] existing = (existing, [])
drawUpTo target (next : rest) existing = drawUpTo target rest (existing ++ [next])

refillHandAfterRemoval :: [PlayingCard] -> [PlayingCard] -> [PlayingCard] -> [PlayingCard] -> Maybe ([PlayingCard], [PlayingCard], [PlayingCard])
refillHandAfterRemoval kept removed currentDraw currentDiscard
  | length currentDraw < cardsNeeded = Nothing
  | otherwise =
      let (drawnCards, finalDraw) = splitAt cardsNeeded currentDraw
       in Just (kept ++ drawnCards, finalDraw, currentDiscard ++ removed)
  where
    cardsNeeded = length removed

partitionByIndices :: [Int] -> [a] -> ([a], [a])
partitionByIndices picked xs =
  foldr split ([], []) (zip [0 ..] xs)
  where
    split (idx, value) (kept, removed) =
      if idx `elem` picked
        then (kept, value : removed)
        else (value : kept, removed)

removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx xs
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

wrapWords :: Int -> String -> [String]
wrapWords maxChars content =
  reverse (foldl addWord [] (words content))
  where
    addWord [] word = [word]
    addWord (current : rest) word
      | length word + 1 + length current <= maxChars = (current ++ " " ++ word) : rest
      | otherwise = word : current : rest

allSame :: Eq a => [a] -> Bool
allSame [] = False
allSame (firstValue : rest) = all (== firstValue) rest

isStraight :: [Rank] -> Bool
isStraight ranks =
  let values = sort (map rankNumber ranks)
      wheel = [2, 3, 4, 5, 14]
   in values == wheel || and (zipWith (\a b -> b - a == 1) values (tail values))

rankNumber :: Rank -> Int
rankNumber rankValue =
  case rankValue of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Queen -> 12
    King -> 13
    Ace -> 14

isFace :: Rank -> Bool
isFace rankValue = rankValue == Jack || rankValue == Queen || rankValue == King

pointsSuffix :: Int -> String
pointsSuffix 0 = "."
pointsSuffix cash = " and earned $" ++ show cash ++ "."

fillerCard :: PlayingCard
fillerCard = PlayingCard Ace Spades NoEnchant

enhancedEnchantments :: [Enchant]
enhancedEnchantments = [BonusChips, LuckyMult, TimesMult, GoldSeal, WildSuit]

jokerEnhancedEnchantments :: [Enchant]
jokerEnhancedEnchantments = [BonusChips, LuckyMult, TimesMult, GoldSeal]

jokerOfferWeight :: [Joker] -> [Joker]
jokerOfferWeight availableJokers =
  availableJokers ++ availableJokers

randomJokerOffers :: RandomGen g => [Joker] -> g -> ([ShopItem], g)
randomJokerOffers [] gen = ([], gen)
randomJokerOffers (joker : rest) gen =
  let (offeredJoker, gen1) = randomizeJokerEnhancement joker gen
      (restOffers, gen2) = randomJokerOffers rest gen1
   in (ShopJoker offeredJoker : restOffers, gen2)

randomizeJokerEnhancement :: RandomGen g => Joker -> g -> (Joker, g)
randomizeJokerEnhancement joker gen =
  let (roll, gen1) = randomR (1 :: Int, 5) gen
   in if roll == 1
        then
          let (idx, gen2) = randomR (0, length jokerEnhancedEnchantments - 1) gen1
           in (joker {jokerEnchant = jokerEnhancedEnchantments !! idx}, gen2)
        else (joker {jokerEnchant = NoEnchant}, gen1)

enhancedCardOfferPool :: [ShopItem]
enhancedCardOfferPool =
  [ ShopCard (PlayingCard rank suit enchantment) (enhancedCardPrice enchantment)
  | rank <- [minBound .. maxBound]
  , suit <- [minBound .. maxBound]
  , enchantment <- enhancedEnchantments
  ]

enhancedCardPrice :: Enchant -> Int
enhancedCardPrice enchantment =
  case enchantment of
    BonusChips -> 4
    LuckyMult -> 4
    TimesMult -> 6
    GoldSeal -> 5
    WildSuit -> 5
    NoEnchant -> 3

randomEnhancedCardOffers :: RandomGen g => Int -> g -> ([ShopItem], g)
randomEnhancedCardOffers count gen =
  let (shuffledOffers, nextGen) = shuffleList enhancedCardOfferPool gen
   in (take count shuffledOffers, nextGen)

shuffleList :: RandomGen g => [a] -> g -> ([a], g)
shuffleList [] gen = ([], gen)
shuffleList xs gen =
  let (idx, gen1) = randomR (0, length xs - 1) gen
      chosen = xs !! idx
      remainder = take idx xs ++ drop (idx + 1) xs
      (shuffledRest, gen2) = shuffleList remainder gen1
   in (chosen : shuffledRest, gen2)

sortHandBy :: (PlayingCard -> PlayingCard -> Ordering) -> Game -> Game
sortHandBy comparator game =
  game
    { handCards = sortBy comparator (handCards game)
    , selectedIndices = []
    , hoveredSortMode = Nothing
    , hoveredCardIndex = Nothing
    }

compareCardsByRank :: PlayingCard -> PlayingCard -> Ordering
compareCardsByRank cardA cardB =
  comparing cardRank cardA cardB <> comparing cardSuit cardA cardB

compareCardsBySuit :: PlayingCard -> PlayingCard -> Ordering
compareCardsBySuit cardA cardB =
  comparing cardSuit cardA cardB <> comparing cardRank cardA cardB

clickedHandIndex :: Game -> (Float, Float) -> Maybe Int
clickedHandIndex game mousePos =
  foldr
    (\idx found ->
        case found of
          Just _ -> found
          Nothing ->
            if pointInRect mousePos (handCardRect idx (idx `elem` selectedIndices game))
              then Just idx
              else Nothing
    )
    Nothing
    [0 .. length (handCards game) - 1]

clickedJokerIndex :: Game -> (Float, Float) -> Maybe Int
clickedJokerIndex game mousePos =
  foldr
    (\idx found ->
        case found of
          Just _ -> found
          Nothing ->
            if idx < length (jokers game) && pointInRect mousePos (jokerSlotRect idx)
              then Just idx
              else Nothing
    )
    Nothing
    [0 .. 4]

hoveredHandCardIndex :: Game -> (Float, Float) -> Maybe Int
hoveredHandCardIndex = clickedHandIndex

handCardRect :: Int -> Bool -> (Float, Float, Float, Float)
handCardRect idx isSelected =
  let centerX = -180 + fromIntegral idx * 88
      centerY = if isSelected then (-185) else (-198)
   in (centerX - 54, centerY - 76, 108, 152)

rankSortButtonRect :: (Float, Float, Float, Float)
rankSortButtonRect = (-405, -220, 90, 40)

suitSortButtonRect :: (Float, Float, Float, Float)
suitSortButtonRect = (-405, -280, 90, 40)

jokerSlotRect :: Int -> (Float, Float, Float, Float)
jokerSlotRect idx =
  let centerX = -110 + fromIntegral idx * 152
      centerY = 250
   in (centerX - 58, centerY - 78, 116, 156)

expandedJokerRect :: (Float, Float, Float, Float)
expandedJokerRect =
  let centerX = 120
      centerY = 30
      width = 116 * 2.45
      height = 156 * 2.45
   in (centerX - width / 2, centerY - height / 2, width, height)

hoveredSortButton :: (Float, Float) -> Maybe SortMode
hoveredSortButton mousePos
  | pointInRect mousePos rankSortButtonRect = Just SortByRank
  | pointInRect mousePos suitSortButtonRect = Just SortBySuit
  | otherwise = Nothing

pointInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
pointInRect (px, py) (left, top, width, height) =
  px >= left && px <= left + width && py >= top && py <= top + height

previewOpacity :: Game -> Float
previewOpacity game =
  min 1 (max 0 (previewTimer game / 2.2))

handUpgradeLevel :: HandType -> Game -> Int
handUpgradeLevel handKind game =
  handUpgradeLevelFrom handKind (handUpgrades game)

handUpgradeLevelFrom :: HandType -> [(HandType, Int)] -> Int
handUpgradeLevelFrom handKind upgrades =
  case lookup handKind upgrades of
    Just level -> level
    Nothing -> 0

upgradeHand :: HandType -> [(HandType, Int)] -> [(HandType, Int)]
upgradeHand handKind [] = [(handKind, 1)]
upgradeHand handKind ((currentKind, level) : rest)
  | handKind == currentKind = (currentKind, level + 1) : rest
  | otherwise = (currentKind, level) : upgradeHand handKind rest

applyHandUpgrade :: [(HandType, Int)] -> HandScore -> HandScore
applyHandUpgrade upgrades score =
  let level = handUpgradeLevelFrom (handType score) upgrades
   in score
        { baseChips = baseChips score + level * upgradeChipsPerLevel
        , baseMult = baseMult score + level * upgradeMultPerLevel
        }

blindPurseIncome :: [Joker] -> Int
blindPurseIncome activeJokers =
  3 * length (filter ((== BlindPurse) . jokerId) activeJokers)

jokerGoldBlindIncome :: [Joker] -> Int
jokerGoldBlindIncome activeJokers =
  3 * length (filter ((== GoldSeal) . jokerEnchant) activeJokers)

jokerGoldSummary :: Int -> String
jokerGoldSummary 0 = ""
jokerGoldSummary goldCash = " including $" ++ show goldCash ++ " joker gold"

extraDiscardCount :: [Joker] -> Int
extraDiscardCount activeJokers =
  length (filter ((== ExtraDiscard) . jokerId) activeJokers)

extraHandCount :: [Joker] -> Int
extraHandCount activeJokers =
  length (filter ((== ExtraHand) . jokerId) activeJokers)

interestIncome :: Int -> Int
interestIncome bankroll =
  min bankroll 25 `div` 5

interestSummary :: Int -> String
interestSummary 0 = ""
interestSummary interestCash = " including $" ++ show interestCash ++ " interest"

jokerSellValue :: Joker -> Int
jokerSellValue joker =
  max 1 ((jokerCost joker * 3 + 5) `div` 10)
