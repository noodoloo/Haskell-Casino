module BalatroMini.Run
  ( startFreshGame
  , startBlind
  , rerollRun
  ) where

import System.Random (mkStdGen, randomR)

import BalatroMini.Cards (drawUpTo)
import BalatroMini.Content (blindLabel, blinds)
import BalatroMini.Jokers (extraDiscardCount, extraHandCount)
import BalatroMini.Types
import BalatroMini.Utils (shuffleList)

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
