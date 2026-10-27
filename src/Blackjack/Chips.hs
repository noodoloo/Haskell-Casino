module Blackjack.Chips
  ( chipButtons
  , clickedChipValue
  ) where

import Graphics.Gloss (Color, makeColorI)

chipButtons :: [(Double, Float, Float, Color)]
chipButtons =
  [ (5, 240, 105, makeColorI 210 60 60 255)
  , (25, 310, 105, makeColorI 55 115 215 255)
  , (100, 380, 105, makeColorI 45 155 95 255)
  , (0, 450, 105, makeColorI 235 235 235 255)
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
