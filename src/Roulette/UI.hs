module Roulette.UI (runRouletteApp) where

import Data.List (find)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Roulette.Logic (applySpin, cycleChip, placeBet, redNumbers, spinWheel)
import Roulette.Types

windowDisplay :: Display
windowDisplay = InWindow "Haskell Roulette" (1280, 860) (100, 100)

backgroundColor :: Color
backgroundColor = makeColorI 14 93 45 255

fps :: Int
fps = 60

cellWidth, cellHeight, boardLeft, boardBottom :: Float
cellWidth = 76
cellHeight = 76
boardLeft = -290
boardBottom = -180

zeroLeft, zeroWidth, sidePanelX :: Float
zeroLeft = boardLeft - 84
zeroWidth = 76
sidePanelX = boardLeft + 565

tablePadding, tableTop, chipTrayTop :: Float
tablePadding = 34
tableTop = boardBottom + (cellHeight * 3) + 46
chipTrayTop = 255

outsideBetBoxes :: [(BetType, (Float, Float, Float, Float), String)]
outsideBetBoxes =
    [ (Red, (boardLeft, boardBottom - 82, 160, 58), "Red")
    , (Black, (boardLeft + 170, boardBottom - 82, 160, 58), "Black")
    , (Even, (boardLeft + 340, boardBottom - 82, 160, 58), "Even")
    , (Odd, (boardLeft, boardBottom - 150, 160, 58), "Odd")
    , (Low, (boardLeft + 170, boardBottom - 150, 160, 58), "1 to 18")
    , (High, (boardLeft + 340, boardBottom - 150, 160, 58), "19 to 36")
    ]

runRouletteApp :: IO ()
runRouletteApp =
    playIO windowDisplay backgroundColor fps initialState render handleEvent stepWorld

render :: GameState -> IO Picture
render st =
    pure $
        pictures
            [ color (greyN 0.95) $ translate (-560) 340 $ scale 0.22 0.22 $ text "Roulette"
            , drawStatus st
            , drawInstructions
            , drawTableFrame
            , drawBoard
            , drawOutsideBets
            , drawPlacedChips st
            , drawChipSelector st
            ]

drawStatus :: GameState -> Picture
drawStatus st =
    let reserved = sum (map amount (activeBets st))
        available = balance st - reserved
     in pictures
            [ translate (-560) 250 $ scale 0.18 0.18 $ color white $ text ("Balance: $" ++ show (balance st))
            , translate (-560) 210 $ scale 0.14 0.14 $ color white $ text ("On table: $" ++ show reserved)
            , translate (-560) 170 $ scale 0.14 0.14 $ color white $ text ("Available: $" ++ show available)
            , translate (-560) 130 $ scale 0.14 0.14 $ color white $ text ("Active bets: " ++ show (length (activeBets st)))
            , translate (-560) 90 $ scale 0.14 0.14 $ color white $ text ("Last spin: " ++ maybe "none" showSpin (lastSpin st))
            , translate (-560) 40 $ scale 0.13 0.13 $ color white $ text (message st)
            ]

drawInstructions :: Picture
drawInstructions =
    pictures
        [ translate (-560) (-60) $
            scale 0.12 0.12 $
                color (light white) $
                    text "Click any number or outside bet to place a chip."
        , translate (-560) (-95) $
            scale 0.12 0.12 $
                color (light white) $
                    text "Press C to cycle chips ($5/$10/$25/$50). Press Enter to spin."
        ]

drawTableFrame :: Picture
drawTableFrame =
    let tableLeft = zeroLeft - tablePadding
        tableRight = boardLeft + (cellWidth * 12) + tablePadding
        tableBottom = boardBottom - 172
        tableWidth = tableRight - tableLeft
        tableHeight = tableTop - tableBottom
        centerX = (tableLeft + tableRight) / 2
        centerY = (tableBottom + tableTop) / 2
     in pictures
            [ color (makeColorI 88 48 22 255) $ translate centerX centerY $ rectangleSolid tableWidth tableHeight
            , color (makeColorI 128 79 38 255) $ translate centerX centerY $ rectangleSolid (tableWidth - 18) (tableHeight - 18)
            , color (makeColorI 12 94 48 255) $ translate centerX centerY $ rectangleSolid (tableWidth - 34) (tableHeight - 34)
            , color (makeColorI 210 185 110 255) $ translate centerX centerY $ rectangleWire (tableWidth - 8) (tableHeight - 8)
            ]

drawBoard :: Picture
drawBoard =
    pictures
        [ drawZeroCell
        , pictures $ concatMap drawRow (zip [0 ..] boardNumbers)
        ]

drawRow :: (Int, [Int]) -> [Picture]
drawRow (rowIndex, nums) =
    map (drawCell rowIndex) (zip [0 ..] nums)

drawZeroCell :: Picture
drawZeroCell =
    let x = zeroLeft + zeroWidth / 2
        y = boardBottom + (cellHeight * 1.5)
     in translate x y $
            pictures
                [ color (makeColorI 25 140 70 255) $ rectangleSolid zeroWidth (cellHeight * 3 - 4)
                , color white $ rectangleWire zeroWidth (cellHeight * 3 - 4)
                , translate (-12) (-8) $ scale 0.16 0.16 $ color white $ text "0"
                ]

drawCell :: Int -> (Int, Int) -> Picture
drawCell rowIndex (colIndex, n) =
    let x = boardLeft + fromIntegral colIndex * cellWidth + cellWidth / 2
        y = boardBottom + fromIntegral rowIndex * cellHeight + cellHeight / 2
        fill =
            if n `elem` redNumbers
                then makeColorI 190 30 45 255
                else makeColorI 20 20 20 255
     in translate x y $
            pictures
                [ color fill $ rectangleSolid (cellWidth - 4) (cellHeight - 4)
                , color white $ rectangleWire (cellWidth - 4) (cellHeight - 4)
                , translate (-14) (-8) $ scale 0.16 0.16 $ color white $ text (show n)
                ]

drawOutsideBets :: Picture
drawOutsideBets = pictures (map drawOutsideBet outsideBetBoxes)

drawOutsideBet :: (BetType, (Float, Float, Float, Float), String) -> Picture
drawOutsideBet (betKind, (leftX, bottomY, width, height), labelText) =
    let x = leftX + width / 2
        y = bottomY + height / 2
        fill = case betKind of
            Red -> makeColorI 180 28 40 255
            Black -> makeColorI 24 24 24 255
            _ -> makeColorI 10 110 50 255
     in translate x y $
            pictures
                [ color fill $ rectangleSolid width height
                , color white $ rectangleWire width height
                , translate (-28) (-8) $ scale 0.13 0.13 $ color white $ text labelText
                ]

drawPlacedChips :: GameState -> Picture
drawPlacedChips st = pictures (map drawChip (activeBets st))

drawChip :: Bet -> Picture
drawChip bet =
    let (x, y) = chipPosition (betType bet)
     in translate x y $
            pictures
                [ color (makeColorI 246 215 70 255) $ thickCircle 9 18
                , color (makeColorI 255 245 180 255) $ circleSolid 12
                , translate (-10) (-5) $ scale 0.09 0.09 $ color black $ text ("$" ++ show (amount bet))
                ]

drawChipSelector :: GameState -> Picture
drawChipSelector st =
    pictures
        [ color (makeColorI 88 48 22 255) $ translate (sidePanelX + 40) 110 $ rectangleSolid 120 340
        , color (makeColorI 12 94 48 255) $ translate (sidePanelX + 40) 110 $ rectangleSolid 98 318
        , color (makeColorI 210 185 110 255) $ translate (sidePanelX + 40) 110 $ rectangleWire 120 340
        , translate sidePanelX 300 $ scale 0.16 0.16 $ color white $ text "Chip Tray"
        , pictures $ zipWith (drawChipOption (selectedChip st)) [0 ..] chipValues
        ]

drawChipOption :: Int -> Int -> Int -> Picture
drawChipOption selected idx chipValue =
    let x = sidePanelX + 45
        y = chipTrayTop - fromIntegral idx * 78
        ringColor
            | selected == chipValue = makeColorI 255 245 180 255
            | otherwise = makeColorI 210 210 210 255
     in translate x y $
            pictures
                [ color (makeColorI 232 179 38 255) $ thickCircle 13 24
                , color ringColor $ circleSolid 18
                , translate (-14) (-6) $ scale 0.11 0.11 $ color black $ text ("$" ++ show chipValue)
                ]

handleEvent :: Event -> GameState -> IO GameState
handleEvent event st =
    case event of
        EventKey (Char 'c') Down _ _ ->
            pure $ cycleChip st
        EventKey (Char 'C') Down _ _ ->
            pure $ cycleChip st
        EventKey (SpecialKey KeyEnter) Down _ _ -> do
            result <- spinWheel
            pure $ applySpin result st
        EventKey (MouseButton LeftButton) Down _ mousePos ->
            pure $ placeBetFromClick mousePos st
        _ ->
            pure st

stepWorld :: Float -> GameState -> IO GameState
stepWorld _ st = pure st

placeBetFromClick :: (Float, Float) -> GameState -> GameState
placeBetFromClick mousePos st =
    case clickedBetType mousePos of
        Nothing -> st
        Just chosenBet ->
            placeBet (Bet chosenBet (selectedChip st)) st

clickedBetType :: (Float, Float) -> Maybe BetType
clickedBetType mousePos =
    case find (pointInBox mousePos . snd3) outsideBetBoxes of
        Just (betKind, _, _) -> Just betKind
        Nothing -> clickedNumberBet mousePos

clickedNumberBet :: (Float, Float) -> Maybe BetType
clickedNumberBet (mx, my)
    | pointInBox (mx, my) (zeroLeft, boardBottom, zeroWidth, cellHeight * 3) =
        Just (StraightUp 0)
    | mx < boardLeft || mx > boardLeft + 12 * cellWidth = Nothing
    | my < boardBottom || my > boardBottom + 3 * cellHeight = Nothing
    | otherwise =
        let col = floor ((mx - boardLeft) / cellWidth)
            row = floor ((my - boardBottom) / cellHeight)
         in fmap StraightUp (safeIndex boardNumbers row >>= safeIndex' col)

chipPosition :: BetType -> (Float, Float)
chipPosition chosenBet =
    case chosenBet of
        StraightUp 0 ->
            (zeroLeft + zeroWidth / 2, boardBottom + (cellHeight * 1.5) + 14)
        StraightUp n ->
            let positions = [(value, chipXY rowIndex colIndex) | (rowIndex, row) <- zip [0 ..] boardNumbers, (colIndex, value) <- zip [0 ..] row]
             in maybe (0, 0) snd (find ((== n) . fst) positions)
        Red -> outsideCenter 0
        Black -> outsideCenter 1
        Even -> outsideCenter 2
        Odd -> outsideCenter 3
        Low -> outsideCenter 4
        High -> outsideCenter 5

chipXY :: Int -> Int -> (Float, Float)
chipXY rowIndex colIndex =
    ( boardLeft + fromIntegral colIndex * cellWidth + cellWidth / 2
    , boardBottom + fromIntegral rowIndex * cellHeight + cellHeight / 2 + 14
    )

outsideCenter :: Int -> (Float, Float)
outsideCenter idx =
    let (_, (leftX, bottomY, width, height), _) = outsideBetBoxes !! idx
     in (leftX + width / 2, bottomY + height / 2)

pointInBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
pointInBox (px, py) (leftX, bottomY, width, height) =
    px >= leftX
        && px <= leftX + width
        && py >= bottomY
        && py <= bottomY + height

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs idx
    | idx < 0 || idx >= length xs = Nothing
    | otherwise = Just (xs !! idx)

safeIndex' :: Int -> [a] -> Maybe a
safeIndex' = flip safeIndex

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

showSpin :: SpinResult -> String
showSpin result =
    let colorName
            | rolledNumber result == 0 = "Green"
            | wasRed result = "Red"
            | otherwise = "Black"
     in show (rolledNumber result) ++ " " ++ colorName
