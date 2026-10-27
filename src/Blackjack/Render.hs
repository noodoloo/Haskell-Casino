module Blackjack.Render
  ( renderGame
  ) where

import Graphics.Gloss

import Blackjack.Cards (handTotal, showRankShort, showSuitShort)
import Blackjack.Chips (chipButtons)
import Blackjack.State (currentPlayerHand, normalizedBet)
import Blackjack.Types
import Blackjack.Utils (formatMoney, trimMessage)

renderGame :: Game -> Picture
renderGame game =
  Pictures
    [ tableBackground
    , titleText
    , controlsText
    , currentTotalText game
    , drawBankrollText game
    , drawBettingChips game
    , drawShoe
    , drawDealerArea game
    , drawPlayerArea game
    , drawHistoryPanel game
    , drawStatusBar game
    ]

tableBackground :: Picture
tableBackground =
  Pictures
    [ Color (makeColorI 7 58 42 255) (Polygon [(-640, -430), (640, -430), (640, 430), (-640, 430)])
    , Color (makeColorI 82 44 28 255) (Polygon [(-590, -330), (590, -330), (590, 330), (-590, 330)])
    , Color (makeColorI 15 96 58 255) (Polygon [(-555, -295), (555, -295), (555, 295), (-555, 295)])
    , Color (makeColorI 206 168 80 255) (lineLoop [(-590, -330), (590, -330), (590, 330), (-590, 330)])
    , Color (makeColorI 235 205 130 255) (lineLoop [(-520, -238), (180, -238), (180, 238), (-520, 238)])
    , Color (makeColorI 9 69 44 180) (translate (-170) (-70) (rectangleSolid 650 290))
    ]

titleText :: Picture
titleText =
  translate (-175) 260 $
    scale 0.32 0.32 $
      color white $
        Text "Blackjack Table"

controlsText :: Picture
controlsText =
  translate (-510) (-320) $
    scale 0.105 0.105 $
      color white $
        Text "M/Esc menu | Deal Enter | H hit | S stand | D double | P split | I insurance | U surrender | N next round"

currentTotalText :: Game -> Picture
currentTotalText game =
  translate (-285) 15 $
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
    [ translate (-500) 205 $ scale 0.18 0.18 $ color (makeColorI 255 235 172 255) $ Text dealerLabel
    , translate (-500) 170 $ scale 0.105 0.105 $ color white $ Text "Dealer hits soft 17"
    , Pictures (zipWith (drawCardPicture game hideSecondCard) [0 ..] (dealerHand game))
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
    [ Color (makeColorI 34 88 74 230) (translate 356 225 (rectangleSolid 310 110))
    , Color (makeColorI 206 168 80 255) (translate 356 225 (rectangleWire 310 110))
    , translate 225 250 $ scale 0.17 0.17 $ color white $ Text ("Chips: $" ++ formatMoney (chips game))
    , translate 225 220 $ scale 0.15 0.15 $ color white $ Text betLabel
    , translate 225 190 $ scale 0.13 0.13 $ color white $ Text insuranceLabel
    ]
  where
    betLabel =
      case phase game of
        Betting -> "Selected bet: $" ++ formatMoney (normalizedBet game)
        _ ->
          case currentPlayerHand game of
            Just hand -> "Active bet: $" ++ formatMoney (betAmount hand)
            Nothing -> "Selected bet: $" ++ formatMoney (normalizedBet game)
    insuranceLabel =
      case insuranceBet game of
        Nothing -> "Insurance: -"
        Just amount -> "Insurance: $" ++ formatMoney amount

drawBettingChips :: Game -> Picture
drawBettingChips _ =
  Pictures
    [ translate 225 152 $ scale 0.115 0.115 $ color white $ Text "Click chips to add to bet"
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
        1 -> [-75]
        2 -> [-235, 85]
        3 -> [-290, -75, 140]
        _ -> [-335, -160, 15, 190]
    drawHand idx hand =
      let xOffset = offsets !! idx
          total = handTotal (cards hand)
          active = phase game == PlayerTurn && idx == activeHandIndex game
          label =
            "Hand "
              ++ show (idx + 1)
              ++ "  Bet $"
              ++ formatMoney (betAmount hand)
              ++ if active then "  active" else ""
              ++ "  Total "
              ++ show total
              ++ if total > 21 then " BUST" else ""
       in Pictures
            [ translate (xOffset - 92) (-28) $ scale 0.13 0.13 $ color white $ Text label
            , translate xOffset (-155) $ drawHandHalo active
            , Pictures (zipWith (drawPlayerCard game xOffset (length (cards hand))) [0 ..] (cards hand))
            ]
    drawBettingPrompt =
      translate (-250) (-55) $
        scale 0.19 0.19 $
          color white $
            Text "Place your bet, then press D."

drawStatusBar :: Game -> Picture
drawStatusBar game =
  Pictures
    [ Color (makeColorI 36 87 76 230) (translate 0 (-248) (rectangleSolid 1040 42))
    , Color (makeColorI 206 168 80 255) (translate 0 (-248) (rectangleWire 1040 42))
    , translate (-500) (-256) $
        scale 0.12 0.12 $
          color white $
            Text (trimMessage 118 (message game))
    ]

drawHandHalo :: Bool -> Picture
drawHandHalo active =
  if active
    then Color (makeColor 1 1 1 0.12) (rectangleSolid 260 230)
    else Blank

drawPlayerCard :: Game -> Float -> Int -> Int -> Card -> Picture
drawPlayerCard game xOffset cardCount idx card =
  let (dx, dy) = cardSlideOffset game (idx == cardCount - 1)
   in translate (xOffset + cardColumnOffset idx + dx) (cardRowOffset idx + dy) (cardFacePicture card)

cardColumnOffset :: Int -> Float
cardColumnOffset idx = -70 + fromIntegral (idx `mod` 4) * 55

cardRowOffset :: Int -> Float
cardRowOffset idx = -105 - fromIntegral (idx `div` 4) * 82

drawCardPicture :: Game -> Bool -> Int -> Card -> Picture
drawCardPicture game hideCard idx card =
  let (dx, dy) = cardSlideOffset game (idx == length (dealerHand game) - 1)
   in translate (-145 + fromIntegral idx * 55 + dx) (135 + dy) $
    if hideCard && idx == 1
      then cardBackPicture
      else cardFacePicture card

cardSlideOffset :: Game -> Bool -> (Float, Float)
cardSlideOffset game isNewest
  | not isNewest = (0, 0)
  | dealTimer game <= 0 = (0, 0)
  | otherwise =
      let t = min 1 (dealTimer game / 0.8)
       in (180 * t, 80 * t)

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

drawShoe :: Picture
drawShoe =
  translate 125 178 $
    Pictures
      [ Color (makeColorI 45 35 32 255) (rectangleSolid 90 62)
      , Color (makeColorI 206 168 80 255) (rectangleWire 90 62)
      , Color (makeColorI 28 28 32 255) (translate (-10) 4 (rectangleSolid 58 42))
      , translate (-30) (-42) $ scale 0.1 0.1 $ color white $ Text "SHOE"
      ]

drawHistoryPanel :: Game -> Picture
drawHistoryPanel game =
  Pictures
    [ Color (makeColorI 34 88 74 230) (translate 357 (-70) (rectangleSolid 310 210))
    , Color (makeColorI 206 168 80 255) (translate 357 (-70) (rectangleWire 310 210))
    , translate 225 20 $ scale 0.14 0.14 $ color (makeColorI 255 235 172 255) $ Text "Round history"
    , Pictures (zipWith drawEntry ([0 ..] :: [Int]) (history game))
    ]
  where
    drawEntry idx entry =
      translate 225 (negate 15 - fromIntegral idx * 32) $
        scale 0.09 0.09 $
          color white $
            Text (trimMessage 42 entry)
