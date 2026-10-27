module BalatroMini.Render.Overlays
  ( drawExpandedJoker
  , drawSellConfirmation
  , drawShopOverlay
  , drawBlindClearedOverlay
  , drawEndOverlay
  , drawHandGuideOverlay
  ) where

import Graphics.Gloss

import BalatroMini.Content (handTypeName, pokerHandGuideRows, upgradeChipsPerLevel, upgradeMultPerLevel)
import BalatroMini.Economy (jokerSellValue)
import BalatroMini.Render.Widgets (cardFacePicture, drawJokerCard, drawWrappedLabel)
import BalatroMini.Scoring (handUpgradeLevel)
import BalatroMini.Types
import BalatroMini.Utils (safeIndex)

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
