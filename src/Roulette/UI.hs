module Roulette.UI (runRouletteApp) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Roulette.Logic (applySpin, placeBet, spinWheel)
import Roulette.Types

windowDisplay :: Display
windowDisplay = InWindow "Haskell Roulette" (1200, 800) (100, 100)

backgroundColor :: Color
backgroundColor = makeColorI 14 93 45 255

fps :: Int
fps = 60

runRouletteApp :: IO ()
runRouletteApp =
    playIO windowDisplay backgroundColor fps initialState render handleEvent stepWorld

render :: GameState -> IO Picture
render st =
    pure $
        pictures
            [ color (greyN 0.95) $ translate (-560) 340 $ scale 0.22 0.22 $ text "Roulette"
            , drawStatus st
            , drawControls
            , drawBoard
            ]

drawStatus :: GameState -> Picture
drawStatus st =
    pictures
        [ translate (-560) 250 $ scale 0.18 0.18 $ color white $ text ("Balance: $" ++ show (balance st))
        , translate (-560) 210 $ scale 0.14 0.14 $ color white $ text ("Current bet: " ++ maybe "none" showBet (currentBet st))
        , translate (-560) 170 $ scale 0.14 0.14 $ color white $ text ("Last spin: " ++ maybe "none" showSpin (lastSpin st))
        , translate (-560) 120 $ scale 0.14 0.14 $ color white $ text (message st)
        ]

drawControls :: Picture
drawControls =
    translate (-560) (-40) $
        scale 0.12 0.12 $
            color (light white) $
                text "1: Red  $25   2: Black  $25   3: Even  $25   4: Odd  $25   5: Low  $25   6: High  $25   7: Straight 17  $10"

drawBoard :: Picture
drawBoard =
    translate (-120) (-280) $
        pictures $
            concatMap drawRow (zip [0 ..] boardNumbers)

drawRow :: (Int, [Int]) -> [Picture]
drawRow (rowIndex, nums) =
    map (drawCell rowIndex) (zip [0 ..] nums)

drawCell :: Int -> (Int, Int) -> Picture
drawCell rowIndex (colIndex, n) =
    let x = fromIntegral colIndex * 80
        y = fromIntegral rowIndex * 80
        fill =
            if n `elem` [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]
                then makeColorI 190 30 45 255
                else makeColorI 20 20 20 255
     in translate x y $
            pictures
                [ color fill $ rectangleSolid 74 74
                , color white $ rectangleWire 74 74
                , translate (-14) (-8) $ scale 0.16 0.16 $ color white $ text (show n)
                ]

handleEvent :: Event -> GameState -> IO GameState
handleEvent event st =
    case event of
        EventKey (Char '1') Down _ _ ->
            pure $ placeBet (Bet Red 25) st
        EventKey (Char '2') Down _ _ ->
            pure $ placeBet (Bet Black 25) st
        EventKey (Char '3') Down _ _ ->
            pure $ placeBet (Bet Even 25) st
        EventKey (Char '4') Down _ _ ->
            pure $ placeBet (Bet Odd 25) st
        EventKey (Char '5') Down _ _ ->
            pure $ placeBet (Bet Low 25) st
        EventKey (Char '6') Down _ _ ->
            pure $ placeBet (Bet High 25) st
        EventKey (Char '7') Down _ _ ->
            pure $ placeBet (Bet (StraightUp 17) 10) st
        EventKey (SpecialKey KeyEnter) Down _ _ -> do
            result <- spinWheel
            pure $ applySpin result st
        _ ->
            pure st

stepWorld :: Float -> GameState -> IO GameState
stepWorld _ st = pure st

showBet :: Bet -> String
showBet (Bet betKind wager) = show betKind ++ " ($" ++ show wager ++ ")"

showSpin :: SpinResult -> String
showSpin result =
    let colorName
            | rolledNumber result == 0 = "Green"
            | wasRed result = "Red"
            | otherwise = "Black"
     in show (rolledNumber result) ++ " " ++ colorName
