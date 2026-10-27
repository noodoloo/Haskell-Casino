module BalatroMini.Render
  ( renderGame
  ) where

import Graphics.Gloss

import BalatroMini.Content (blindLabel, blinds, handTypeName, totalAntes)
import BalatroMini.HandEval (evaluateBestHand)
import BalatroMini.Render.Overlays
  ( drawBlindClearedOverlay
  , drawEndOverlay
  , drawExpandedJoker
  , drawHandGuideOverlay
  , drawSellConfirmation
  , drawShopOverlay
  )
import BalatroMini.Render.Widgets (cardFacePicture, drawJokerCard, drawWrappedLabel)
import BalatroMini.Types
import BalatroMini.Utils (safeIndex)

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

previewOpacity :: Game -> Float
previewOpacity game =
  min 1 (max 0 (previewTimer game / 2.2))
