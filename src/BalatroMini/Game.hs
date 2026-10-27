module BalatroMini.Game
  ( Game
  , balatroStartingMoney
  , balatroCasinoPayout
  , startFreshGame
  , renderGame
  , handleEvent
  , stepGame
  ) where

import BalatroMini.Input (handleEvent)
import BalatroMini.Render (renderGame)
import BalatroMini.Run (startFreshGame)
import BalatroMini.Types

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
