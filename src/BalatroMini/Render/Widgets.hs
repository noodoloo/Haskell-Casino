module BalatroMini.Render.Widgets
  ( cardFacePicture
  , drawWrappedLabel
  , drawJokerCard
  , jokerEnchantLabel
  , enchantDisplayColor
  ) where

import Graphics.Gloss

import BalatroMini.Cards (enchantShort, rankLabel, suitGlyph)
import BalatroMini.Types
import BalatroMini.Utils (wrapWords)

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
