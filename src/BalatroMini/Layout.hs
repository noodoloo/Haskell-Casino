module BalatroMini.Layout
  ( clickedHandIndex
  , clickedJokerIndex
  , hoveredHandCardIndex
  , handCardRect
  , rankSortButtonRect
  , suitSortButtonRect
  , jokerSlotRect
  , expandedJokerRect
  , hoveredSortButton
  , pointInRect
  ) where

import BalatroMini.Types

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
