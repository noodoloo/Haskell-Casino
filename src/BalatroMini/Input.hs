module BalatroMini.Input
  ( handleEvent
  ) where

import Graphics.Gloss.Interface.Pure.Game

import BalatroMini.Cards (compareCardsByRank, compareCardsBySuit)
import BalatroMini.Layout
  ( clickedHandIndex
  , clickedJokerIndex
  , expandedJokerRect
  , hoveredHandCardIndex
  , hoveredSortButton
  , pointInRect
  , rankSortButtonRect
  , suitSortButtonRect
  )
import BalatroMini.Round
  ( advanceBlind
  , discardSelected
  , playSelectedHand
  , requestSellExpandedJoker
  , sellJoker
  , sortHandBy
  , toggleExpandedJoker
  , toggleIndex
  )
import BalatroMini.Run (rerollRun)
import BalatroMini.Shop (buyShopItem, leaveShop)
import BalatroMini.Types

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
