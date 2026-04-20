module Blackjack
  ( Game
  , initialGame
  , runBlackjackApp
  , renderGame
  , handleEvent
  , stepGame
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Numeric (showFFloat)
import System.Random (RandomGen, StdGen, newStdGen, randomR)

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
  , message :: String
  , generator :: StdGen
  }

initialGame :: IO Game
initialGame = do
  gen <- newStdGen
  pure (initialLobby gen)

runBlackjackApp :: IO ()
runBlackjackApp = do
  game <- initialGame
  play
    (InWindow "Haskell Casino - Blackjack" (1200, 680) (100, 80))
    black
    60
    game
    renderGame
    handleEvent
    stepGame

stepGame :: Float -> Game -> Game
stepGame _ = id

handleEvent :: Event -> Game -> Game
handleEvent event game =
  case phase game of
    Betting -> handleBettingEvent event game
    PlayerTurn -> handlePlayerTurnEvent event game
    RoundFinished -> handleRoundFinishedEvent event game

renderGame :: Game -> Picture
renderGame game =
  Pictures
    [ tableBackground
    , titleText
    , controlsText
    , currentTotalText game
    , drawBankrollText game
    , drawBettingChips game
    , drawDealerArea game
    , drawPlayerArea game
    , drawStatusBar game
    ]

initialLobby :: StdGen -> Game
initialLobby gen =
  Game
    { deck = []
    , dealerHand = []
    , playerHands = []
    , activeHandIndex = 0
    , phase = Betting
    , chips = 500
    , pendingBet = 25
    , message = "Choose your bet with Up/Down, then press Enter to deal."
    , generator = gen
    }

startRound :: Game -> Game
startRound game =
  let (shuffledDeck, gen1) = shuffleDeck fullDeck gen0
      (playerCard1, deck1) = drawCard shuffledDeck
      (dealerCard1, deck2) = drawCard deck1
      (playerCard2, deck3) = drawCard deck2
      (dealerCard2, deck4) = drawCard deck3
      wager = normalizedBet game
      dealtGame =
        game
        { deck = deck4
        , dealerHand = [dealerCard1, dealerCard2]
        , playerHands = [PlayerHand [playerCard1, playerCard2] False wager True]
        , activeHandIndex = 0
        , phase = PlayerTurn
        , chips = chips game - wager
        , pendingBet = wager
        , message = "Your turn. H = hit, S = stand, P = split when allowed."
        , generator = gen1
        }
   in resolveOpeningNaturals dealtGame
  where
    gen0 = generator game

handleBettingEvent :: Event -> Game -> Game
handleBettingEvent event game =
  case event of
    EventKey (MouseButton LeftButton) Down _ mousePos -> handleChipClick mousePos game
    EventKey (SpecialKey KeyUp) Down _ _ -> changePendingBet 5 game
    EventKey (SpecialKey KeyDown) Down _ _ -> changePendingBet (-5) game
    EventKey (Char '+') Down _ _ -> changePendingBet 5 game
    EventKey (Char '-') Down _ _ -> changePendingBet (-5) game
    EventKey (Char 'd') Down _ _ -> dealIfPossible game
    EventKey (Char 'D') Down _ _ -> dealIfPossible game
    EventKey (SpecialKey KeyEnter) Down _ _ -> dealIfPossible game
    EventKey (SpecialKey KeySpace) Down _ _ -> dealIfPossible game
    _ -> game

handlePlayerTurnEvent :: Event -> Game -> Game
handlePlayerTurnEvent event game =
  case event of
    EventKey (Char 'h') Down _ _ -> playerHit game
    EventKey (Char 'H') Down _ _ -> playerHit game
    EventKey (Char 's') Down _ _ -> playerStand game
    EventKey (Char 'S') Down _ _ -> playerStand game
    EventKey (Char 'd') Down _ _ -> playerDoubleDown game
    EventKey (Char 'D') Down _ _ -> playerDoubleDown game
    EventKey (Char 'p') Down _ _ -> playerSplit game
    EventKey (Char 'P') Down _ _ -> playerSplit game
    EventKey (SpecialKey KeySpace) Down _ _ -> playerStand game
    _ -> game

handleRoundFinishedEvent :: Event -> Game -> Game
handleRoundFinishedEvent event game =
  case event of
    EventKey (Char 'n') Down _ _ -> returnToBetting game
    EventKey (Char 'N') Down _ _ -> returnToBetting game
    EventKey (Char 'd') Down _ _ -> dealIfPossible (returnToBetting game)
    EventKey (Char 'D') Down _ _ -> dealIfPossible (returnToBetting game)
    _ -> game

returnToBetting :: Game -> Game
returnToBetting game =
  game
    { deck = []
    , dealerHand = []
    , playerHands = []
    , activeHandIndex = 0
    , phase = Betting
    , pendingBet = normalizedBet game
    , message = "Choose your bet with Up/Down, then press D to deal."
    }

dealIfPossible :: Game -> Game
dealIfPossible game
  | chips game <= 0 =
      game {message = "You are out of chips. Restart the game to play again."}
  | normalizedBet game > chips game =
      game {message = "Your bet cannot be higher than your current chips."}
  | otherwise = startRound game

changePendingBet :: Double -> Game -> Game
changePendingBet delta game =
  game
    { pendingBet = clampBet (chips game) (pendingBet game + delta)
    , message = "Choose your bet with Up/Down, then press D to deal."
    }

handleChipClick :: (Float, Float) -> Game -> Game
handleChipClick mousePos game =
  case clickedChipValue mousePos of
    Just 0 ->
      game
        { pendingBet = clampBet (chips game) 5
        , message = "Bet reset. Click chips or use Up/Down, then press D to deal."
        }
    Just amount ->
      game
        { pendingBet = clampBet (chips game) (pendingBet game + amount)
        , message = "Added $" ++ formatMoney amount ++ " to the bet. Press D to deal when ready."
        }
    Nothing -> game

playerHit :: Game -> Game
playerHit game
  | phase game /= PlayerTurn = game
  | not (hasActiveHand game) = game
  | otherwise =
      let (newCard, newDeck) = drawCard (deck game)
          updatedHands = modifyActiveHand game (\hand -> hand {cards = cards hand ++ [newCard]})
          activeHand = updatedHands !! activeHandIndex game
          busted = handTotal (cards activeHand) > 21
          charlie = hasFiveCardCharlie activeHand
          nextGame =
            game
              { deck = newDeck
              , playerHands =
                  if busted || charlie
                    then markActiveStanding updatedHands (activeHandIndex game)
                    else updatedHands
              , message =
                  if busted
                    then "Hand " ++ show (activeHandIndex game + 1) ++ " busted after drawing " ++ showCard newCard ++ "."
                    else
                      if charlie
                        then "Hand " ++ show (activeHandIndex game + 1) ++ " made a 5-card Charlie with " ++ showCard newCard ++ "."
                        else "Hand " ++ show (activeHandIndex game + 1) ++ " drew " ++ showCard newCard ++ "."
              }
       in if busted || charlie then advanceAfterAction nextGame else nextGame

playerStand :: Game -> Game
playerStand game
  | phase game /= PlayerTurn = game
  | not (hasActiveHand game) = game
  | otherwise =
      let updatedHands = markActiveStanding (playerHands game) (activeHandIndex game)
          stoodGame =
            game
              { playerHands = updatedHands
              , message = "Hand " ++ show (activeHandIndex game + 1) ++ " stands."
              }
       in advanceAfterAction stoodGame

playerSplit :: Game -> Game
playerSplit game
  | phase game /= PlayerTurn = game
  | not (hasEnoughChipsToSplit game) =
      game {message = "You need enough chips to match the current hand bet before splitting."}
  | not (canSplitActiveHand game) =
      game {message = "You can only split a hand with exactly two cards of the same rank."}
  | otherwise =
      let hand = playerHands game !! activeHandIndex game
       in case cards hand of
            [firstCard, secondCard] ->
              let (drawnOne, deck1) = drawCard (deck game)
                  (drawnTwo, deck2) = drawCard deck1
                  newHandA = PlayerHand [firstCard, drawnOne] False (betAmount hand) False
                  newHandB = PlayerHand [secondCard, drawnTwo] False (betAmount hand) False
                  updatedHands =
                    replaceAt
                      (activeHandIndex game)
                      [newHandA, newHandB]
                      (playerHands game)
               in game
                    { deck = deck2
                    , chips = chips game - betAmount hand
                    , playerHands = updatedHands
                    , message = "Hand split into two hands. The matching bet was added automatically."
                    }
            _ -> game {message = "You can only split a hand with exactly two cards."}

playerDoubleDown :: Game -> Game
playerDoubleDown game
  | phase game /= PlayerTurn = game
  | not (canDoubleDownActiveHand game) =
      game {message = "You can only double down on an active hand that has exactly two cards."}
  | not (hasEnoughChipsToDouble game) =
      game {message = "You need enough chips to match the current hand bet before doubling down."}
  | otherwise =
      let idx = activeHandIndex game
          hand = playerHands game !! idx
          (newCard, newDeck) = drawCard (deck game)
          doubledHand =
            hand
              { cards = cards hand ++ [newCard]
              , isStanding = True
              , betAmount = betAmount hand * 2
              }
          updatedHands =
            take idx (playerHands game) ++ [doubledHand] ++ drop (idx + 1) (playerHands game)
          doubledGame =
            game
              { deck = newDeck
              , chips = chips game - betAmount hand
              , playerHands = updatedHands
              , message =
                  "Hand "
                    ++ show (idx + 1)
                    ++ " doubled down and drew "
                    ++ showCard newCard
                    ++ "."
              }
       in advanceAfterAction doubledGame

advanceAfterAction :: Game -> Game
advanceAfterAction game =
  case nextPlayableHandIndex (playerHands game) (activeHandIndex game + 1) of
    Just idx -> game {activeHandIndex = idx}
    Nothing -> finishDealerTurn game

finishDealerTurn :: Game -> Game
finishDealerTurn game =
  let finalDealer = playDealer (deck game) (dealerHand game)
      finalHands = playerHands game
      payout = sum (map (settlementForHand finalDealer) finalHands)
      resultLines = zipWith (describeOutcome finalDealer) [1 ..] finalHands
   in game
        { dealerHand = finalDealer
        , phase = RoundFinished
        , activeHandIndex = 0
        , chips = chips game + payout
        , message = unwords resultLines ++ " Press N for the next betting round."
        }

playDealer :: [Card] -> [Card] -> [Card]
playDealer currentDeck currentHand
  | dealerShouldHit currentHand =
      let (nextCard, remainingDeck) = drawCard currentDeck
       in playDealer remainingDeck (currentHand ++ [nextCard])
  | otherwise = currentHand

dealerShouldHit :: [Card] -> Bool
dealerShouldHit hand =
  let total = handTotal hand
   in total < 17 || (total == 17 && isSoft hand)

describeOutcome :: [Card] -> Int -> PlayerHand -> String
describeOutcome dealer idx hand
  | hasNaturalBlackjack hand && hasNaturalBlackjack dealerHandState = "Hand " ++ show idx ++ " pushes with blackjack."
  | hasNaturalBlackjack hand = "Hand " ++ show idx ++ " has blackjack and wins $" ++ formatMoney (wager * 1.5) ++ "."
  | hasFiveCardCharlie hand = "Hand " ++ show idx ++ " wins with a 5-card Charlie."
  | playerScore > 21 = "Hand " ++ show idx ++ " loses $" ++ formatMoney wager ++ "."
  | dealerScore > 21 = "Hand " ++ show idx ++ " wins $" ++ formatMoney wager ++ "."
  | hasNaturalBlackjack dealerHandState = "Hand " ++ show idx ++ " loses to dealer blackjack."
  | playerScore > dealerScore = "Hand " ++ show idx ++ " wins $" ++ formatMoney wager ++ "."
  | playerScore < dealerScore = "Hand " ++ show idx ++ " loses $" ++ formatMoney wager ++ "."
  | otherwise = "Hand " ++ show idx ++ " pushes."
  where
    playerScore = handTotal (cards hand)
    dealerScore = handTotal dealer
    wager = betAmount hand
    dealerHandState = PlayerHand dealer False 0 False

settlementForHand :: [Card] -> PlayerHand -> Double
settlementForHand dealer hand
  | hasNaturalBlackjack hand && hasNaturalBlackjack dealerHandState = wager
  | hasNaturalBlackjack hand = wager * 2.5
  | hasFiveCardCharlie hand = wager * 2
  | playerScore > 21 = 0
  | hasNaturalBlackjack dealerHandState = 0
  | dealerScore > 21 = wager * 2
  | playerScore > dealerScore = wager * 2
  | playerScore < dealerScore = 0
  | otherwise = wager
  where
    playerScore = handTotal (cards hand)
    dealerScore = handTotal dealer
    wager = betAmount hand
    dealerHandState = PlayerHand dealer False 0 False

resolveOpeningNaturals :: Game -> Game
resolveOpeningNaturals game
  | playerNatural || dealerNatural =
      let finalHands = playerHands game
          finalDealer = dealerHand game
          payout = sum (map (settlementForHand finalDealer) finalHands)
          resultLines = zipWith (describeOutcome finalDealer) [1 ..] finalHands
       in game
            { phase = RoundFinished
            , chips = chips game + payout
            , message = unwords resultLines ++ " Press N for the next betting round."
            }
  | otherwise = game
  where
    playerNatural = any hasNaturalBlackjack (playerHands game)
    dealerNatural = hasNaturalBlackjack (PlayerHand (dealerHand game) False 0 False)

hasActiveHand :: Game -> Bool
hasActiveHand game =
  activeHandIndex game >= 0 && activeHandIndex game < length (playerHands game)

hasEnoughChipsToSplit :: Game -> Bool
hasEnoughChipsToSplit game =
  case currentPlayerHand game of
    Just hand -> chips game >= betAmount hand
    Nothing -> False

hasEnoughChipsToDouble :: Game -> Bool
hasEnoughChipsToDouble game =
  case currentPlayerHand game of
    Just hand -> chips game >= betAmount hand
    Nothing -> False

currentPlayerHand :: Game -> Maybe PlayerHand
currentPlayerHand game
  | hasActiveHand game = Just (playerHands game !! activeHandIndex game)
  | otherwise = Nothing

modifyActiveHand :: Game -> (PlayerHand -> PlayerHand) -> [PlayerHand]
modifyActiveHand game f =
  let idx = activeHandIndex game
   in take idx (playerHands game) ++ [f (playerHands game !! idx)] ++ drop (idx + 1) (playerHands game)

markActiveStanding :: [PlayerHand] -> Int -> [PlayerHand]
markActiveStanding hands idx =
  take idx hands
    ++ [ (hands !! idx) {isStanding = True} ]
    ++ drop (idx + 1) hands

nextPlayableHandIndex :: [PlayerHand] -> Int -> Maybe Int
nextPlayableHandIndex hands startIdx =
  case filter (\idx -> not (isStanding (hands !! idx)) && handTotal (cards (hands !! idx)) <= 21) [startIdx .. length hands - 1] of
    idx : _ -> Just idx
    [] -> Nothing

canSplitActiveHand :: Game -> Bool
canSplitActiveHand game =
  case currentPlayerHand game of
    Just hand ->
      case cards hand of
        [cardA, cardB] -> rank cardA == rank cardB
        _ -> False
    Nothing -> False

canDoubleDownActiveHand :: Game -> Bool
canDoubleDownActiveHand game =
  case currentPlayerHand game of
    Just hand -> not (isStanding hand) && length (cards hand) == 2
    Nothing -> False

normalizedBet :: Game -> Double
normalizedBet game = clampBet (chips game) (pendingBet game)

clampBet :: Double -> Double -> Double
clampBet available requested
  | available <= 0 = 0
  | available < 5 = available
  | otherwise = max 5 (min available requested)

hasNaturalBlackjack :: PlayerHand -> Bool
hasNaturalBlackjack hand =
  blackjackEligible hand && length (cards hand) == 2 && handTotal (cards hand) == 21

hasFiveCardCharlie :: PlayerHand -> Bool
hasFiveCardCharlie hand =
  length (cards hand) >= 5 && handTotal (cards hand) <= 21

formatMoney :: Double -> String
formatMoney amount =
  if abs (amount - fromIntegral (round amount :: Int)) < 0.001
    then show (round amount :: Int)
    else showFFloat (Just 1) amount ""

handTotal :: [Card] -> Int
handTotal hand = adjustForAces rawTotal aceCount
  where
    values = map cardHighValue hand
    rawTotal = sum values
    aceCount = length (filter ((== Ace) . rank) hand)

isSoft :: [Card] -> Bool
isSoft hand =
  aceCount > 0 && bestTotal <= 21 && bestTotal /= hardTotal
  where
    bestTotal = handTotal hand
    hardTotal = sum (map cardLowValue hand)
    aceCount = length (filter ((== Ace) . rank) hand)

adjustForAces :: Int -> Int -> Int
adjustForAces total aceCount
  | total <= 21 = total
  | aceCount == 0 = total
  | otherwise = adjustForAces (total - 10) (aceCount - 1)

cardHighValue :: Card -> Int
cardHighValue card =
  case rank card of
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

cardLowValue :: Card -> Int
cardLowValue card =
  case rank card of
    Ace -> 1
    _ -> cardHighValue card

fullDeck :: [Card]
fullDeck = [Card r s | s <- [minBound .. maxBound], r <- [minBound .. maxBound]]

shuffleDeck :: RandomGen g => [Card] -> g -> ([Card], g)
shuffleDeck [] gen = ([], gen)
shuffleDeck xs gen =
  let (idx, nextGen) = randomR (0, length xs - 1) gen
      chosen = xs !! idx
      remainder = take idx xs ++ drop (idx + 1) xs
      (rest, finalGen) = shuffleDeck remainder nextGen
   in (chosen : rest, finalGen)

drawCard :: [Card] -> (Card, [Card])
drawCard [] = error "Deck unexpectedly empty."
drawCard (card : rest) = (card, rest)

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt idx replacement xs = take idx xs ++ replacement ++ drop (idx + 1) xs

showCard :: Card -> String
showCard (Card r s) = showRankLong r ++ " of " ++ showSuitLong s

showRankLong :: Rank -> String
showRankLong r =
  case r of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"
    Ace -> "Ace"

showRankShort :: Rank -> String
showRankShort r =
  case r of
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

showSuitLong :: Suit -> String
showSuitLong s =
  case s of
    Clubs -> "Clubs"
    Diamonds -> "Diamonds"
    Hearts -> "Hearts"
    Spades -> "Spades"

showSuitShort :: Suit -> String
showSuitShort s =
  case s of
    Clubs -> "C"
    Diamonds -> "D"
    Hearts -> "H"
    Spades -> "S"

tableBackground :: Picture
tableBackground =
  Pictures
    [ Color (makeColorI 10 80 40 255) (Polygon [(-600, -340), (600, -340), (600, 340), (-600, 340)])
    , Color (makeColorI 180 140 60 255) (lineLoop [(-560, -300), (560, -300), (560, 300), (-560, 300)])
    ]

titleText :: Picture
titleText =
  translate (-180) 275 $
    scale 0.35 0.35 $
      color white $
        Text "Blackjack Table"

controlsText :: Picture
controlsText =
  translate (-550) (-320) $
    scale 0.13 0.13 $
      color white $
        Text "Betting: Up/Down change bet, Deal with Enter | Playing: H hit, S stand, D double, P split | N next round"

currentTotalText :: Game -> Picture
currentTotalText game =
  translate (-210) 15 $
    scale 0.18 0.18 $
      color white $
        Text totalLabel
  where
    totalLabel =
      case currentPlayerHand game of
        Just hand -> "Current hand total: " ++ show (handTotal (cards hand))
        Nothing -> "Current hand total: -"

drawDealerArea :: Game -> Picture
drawDealerArea game =
  Pictures
    [ translate (-520) 180 $ scale 0.18 0.18 $ color white $ Text dealerLabel
    , Pictures (zipWith (drawCardPicture hideSecondCard) [0 ..] (dealerHand game))
    ]
  where
    hideSecondCard = phase game == PlayerTurn
    visibleDealerTotal =
      if hideSecondCard
        then handTotal (take 1 (dealerHand game))
        else handTotal (dealerHand game)
    dealerLabel = "Dealer: " ++ show visibleDealerTotal

drawBankrollText :: Game -> Picture
drawBankrollText game =
  Pictures
    [ translate (215) 245 $ scale 0.18 0.18 $ color white $ Text ("Chips: $" ++ formatMoney (chips game))
    , translate (215) 215 $ scale 0.18 0.18 $ color white $ Text betLabel
    ]
  where
    betLabel =
      case phase game of
        Betting -> "Selected bet: $" ++ formatMoney (normalizedBet game)
        _ ->
          case currentPlayerHand game of
            Just hand -> "Active bet: $" ++ formatMoney (betAmount hand)
            Nothing -> "Selected bet: $" ++ formatMoney (normalizedBet game)

drawBettingChips :: Game -> Picture
drawBettingChips _ =
  Pictures
    [ translate 325 165 $ scale 0.15 0.15 $ color white $ Text "Click chips to add to bet"
    , Pictures (map drawChip chipButtons)
    ]
  where
    drawChip (amount, xPos, yPos, chipColor) =
      translate xPos yPos $
        Pictures
          [ color chipColor (ThickCircle 12 24)
          , color white (circle 12)
          , translate labelX (-5) $ scale labelScale labelScale $ color black $ Text chipLabel
          ]
      where
        chipLabel =
          if amount == 0
            then "CLR"
            else "$" ++ formatMoney amount
        labelScale =
          if amount == 0
            then 0.12
            else 0.14
        labelX =
          if amount == 0
            then -16
            else -14

drawPlayerArea :: Game -> Picture
drawPlayerArea game =
  Pictures $
    [drawBettingPrompt | phase game == Betting]
      ++ zipWith drawHand [0 ..] (playerHands game)
  where
    handCount = length (playerHands game)
    offsets =
      case handCount of
        1 -> [0]
        2 -> [-180, 180]
        3 -> [-240, 0, 240]
        _ -> [-300, -100, 100, 300]
    drawHand idx hand =
      let xOffset = offsets !! idx
          total = handTotal (cards hand)
          active = phase game == PlayerTurn && idx == activeHandIndex game
          label =
            "Hand "
              ++ show (idx + 1)
              ++ " | Bet: $"
              ++ formatMoney (betAmount hand)
              ++ if active then " <- active" else ""
              ++ " | Total: "
              ++ show total
              ++ if total > 21 then " BUST" else ""
       in Pictures
            [ translate (xOffset - 95) (-25) $ scale 0.16 0.16 $ color white $ Text label
            , translate xOffset (-155) $ drawHandHalo active
            , Pictures (zipWith (drawPlayerCard xOffset) [0 ..] (cards hand))
            ]
    drawBettingPrompt =
      translate (-300) (-55) $
        scale 0.23 0.23 $
          color white $
            Text "Place your bet, then press D to deal."

drawStatusBar :: Game -> Picture
drawStatusBar game =
  translate (-550) (-290) $
    scale 0.14 0.14 $
      color white $
        Text (message game)

drawHandHalo :: Bool -> Picture
drawHandHalo active =
  if active
    then Color (makeColor 1 1 1 0.12) (rectangleSolid 260 230)
    else Blank

drawPlayerCard :: Float -> Int -> Card -> Picture
drawPlayerCard xOffset idx card =
  translate (xOffset + cardColumnOffset idx) (cardRowOffset idx) (cardFacePicture card)

cardColumnOffset :: Int -> Float
cardColumnOffset idx = -70 + fromIntegral (idx `mod` 4) * 55

cardRowOffset :: Int -> Float
cardRowOffset idx = -105 - fromIntegral (idx `div` 4) * 82

drawCardPicture :: Bool -> Int -> Card -> Picture
drawCardPicture hideCard idx card =
  translate (-70 + fromIntegral idx * 55) 135 $
    if hideCard && idx == 1
      then cardBackPicture
      else cardFacePicture card

cardFacePicture :: Card -> Picture
cardFacePicture card =
  Pictures
    [ Color white (rectangleSolid 48 68)
    , Color black (rectangleWire 48 68)
    , translate (-16) 8 $ scale 0.14 0.14 $ color inkColor $ Text (showRankShort (rank card))
    , translate (-16) (-16) $ scale 0.14 0.14 $ color inkColor $ Text (showSuitShort (suit card))
    ]
  where
    inkColor =
      case suit card of
        Hearts -> red
        Diamonds -> red
        _ -> black

cardBackPicture :: Picture
cardBackPicture =
  Pictures
    [ Color (makeColorI 30 70 160 255) (rectangleSolid 48 68)
    , Color white (rectangleWire 48 68)
    , Color (makeColorI 180 210 255 255) (rectangleWire 36 56)
    ]

chipButtons :: [(Double, Float, Float, Color)]
chipButtons =
  [ (5, 430, 110, makeColorI 210 60 60 255)
  , (25, 430, 45, makeColorI 55 115 215 255)
  , (100, 430, (-20), makeColorI 45 155 95 255)
  , (0, 430, (-95), makeColorI 235 235 235 255)
  ]

clickedChipValue :: (Float, Float) -> Maybe Double
clickedChipValue (mouseX, mouseY) =
  case filter chipWasClicked chipButtons of
    (amount, _, _, _) : _ -> Just amount
    [] -> Nothing
  where
    chipWasClicked (_, chipX, chipY, _) =
      let dx = mouseX - chipX
          dy = mouseY - chipY
       in dx * dx + dy * dy <= 36 * 36
