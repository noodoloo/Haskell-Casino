module Slots.UI (runSlotsApp) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Slots.Logic (changeBet, maxBet, resetSlots, spinSlots)
import Slots.Types

windowDisplay :: Display
windowDisplay = InWindow "Haskell Casino - Slots" (1100, 720) (120, 100)

backgroundColor :: Color
backgroundColor = makeColorI 18 58 48 255

fps :: Int
fps = 60

runSlotsApp :: IO ()
runSlotsApp =
    playIO windowDisplay backgroundColor fps initialState render handleEvent stepWorld

render :: GameState -> IO Picture
render st =
    pure $
        pictures
            [ drawCabinet
            , drawHeader
            , drawReels st
            , drawHud st
            , drawControls
            , drawPaytable
            ]

drawCabinet :: Picture
drawCabinet =
    pictures
        [ color (makeColorI 76 32 48 255) $ rectangleSolid 900 585
        , color (makeColorI 232 187 82 255) $ rectangleWire 900 585
        , color (makeColorI 30 31 34 255) $ translate (-95) 45 $ rectangleSolid 585 238
        , color (makeColorI 232 187 82 255) $ translate (-95) 45 $ rectangleWire 585 238
        , color (makeColorI 36 87 76 255) $ translate (-95) (-198) $ rectangleSolid 585 104
        , color (makeColorI 232 187 82 255) $ translate (-95) (-198) $ rectangleWire 585 104
        , color (makeColorI 40 64 66 255) $ translate 315 45 $ rectangleSolid 210 360
        , color (makeColorI 232 187 82 255) $ translate 315 45 $ rectangleWire 210 360
        , pictures (map drawTopLight [-360, -300 .. 360])
        ]

drawTopLight :: Float -> Picture
drawTopLight x =
    translate x 298 $
        pictures
            [ color (makeColorI 236 202 93 255) $ circleSolid 8
            , color (makeColorI 255 244 174 255) $ circleSolid 4
            ]

drawHeader :: Picture
drawHeader =
    pictures
        [ drawLabel (-225) 238 0.42 (makeColorI 255 235 172 255) "Haskell Slots"
        , drawLabel (-258) 204 0.13 white "Cherry pays often. Sevens pay loud."
        ]

drawReels :: GameState -> Picture
drawReels st =
    pictures (zipWith drawReel [-1, 0, 1] shownSymbols)
  where
    shownSymbols =
        case lastSpin st of
            Just result -> reels result
            Nothing -> [Cherry, Seven, Bar]

drawReel :: Int -> Symbol -> Picture
drawReel idx symbol =
    let x = -95 + fromIntegral idx * 170
     in translate x 48 $
            pictures
                [ color (makeColorI 246 240 222 255) $ rectangleSolid 132 166
                , color (makeColorI 28 28 30 255) $ rectangleWire 132 166
                , drawSymbol symbol
                , translate (-46) (-70) $ scale 0.095 0.095 $ color (makeColorI 35 35 35 255) $ text (symbolName symbol)
                ]

drawSymbol :: Symbol -> Picture
drawSymbol symbol =
    case symbol of
        Cherry ->
            pictures
                [ color (makeColorI 190 30 54 255) $ translate (-18) 12 $ circleSolid 22
                , color (makeColorI 220 42 62 255) $ translate 18 12 $ circleSolid 22
                , color (makeColorI 34 120 54 255) $ translate 14 42 $ rotate (-35) $ rectangleSolid 10 42
                ]
        Lemon ->
            pictures
                [ color (makeColorI 238 212 58 255) $ scale 1.35 0.82 $ circleSolid 34
                , color (makeColorI 105 120 42 255) $ circle 34
                ]
        Bell ->
            pictures
                [ color (makeColorI 224 172 54 255) $ translate 0 4 $ thickArc 200 340 42 34
                , color (makeColorI 236 196 86 255) $ translate 0 (-18) $ rectangleSolid 74 34
                , color (makeColorI 95 64 28 255) $ translate 0 (-39) $ circleSolid 8
                ]
        Bar ->
            pictures
                [ color (makeColorI 20 20 22 255) $ rectangleSolid 92 44
                , color white $ rectangleWire 92 44
                , translate (-27) (-10) $ scale 0.18 0.18 $ color white $ text "BAR"
                ]
        Seven ->
            translate (-24) (-36) $ scale 0.68 0.68 $ color (makeColorI 186 28 38 255) $ text "7"
        Diamond ->
            pictures
                [ color (makeColorI 74 190 210 255) $ polygon [(0, 48), (44, 0), (0, -48), (-44, 0)]
                , color white $ lineLoop [(0, 48), (44, 0), (0, -48), (-44, 0)]
                ]

drawHud :: GameState -> Picture
drawHud st =
    pictures
        [ drawLabel (-360) (-164) 0.16 white ("Balance: $" ++ show (balance st))
        , drawLabel (-165) (-164) 0.16 white ("Bet: $" ++ show (currentBet st))
        , drawLabel 5 (-164) 0.16 white lastPayout
        , drawLabel (-360) (-223) 0.12 white (trimMessage 72 (message st))
        , drawSpinButton
        ]
  where
    lastPayout =
        case lastSpin st of
            Nothing -> "Last payout: -"
            Just result -> "Last payout: $" ++ show (payout result)

drawSpinButton :: Picture
drawSpinButton =
    translate 155 (-230) $
        pictures
            [ color (makeColorI 196 42 58 255) $ circleSolid 39
            , color white $ thickCircle 33 6
            , translate (-22) (-8) $ scale 0.115 0.115 $ color white $ text "SPIN"
            ]

drawControls :: Picture
drawControls =
    pictures
        [ drawLabel (-405) (-325) 0.12 white "Space/Enter spin | Up/Down bet | M max bet | R reset | Click SPIN"
        ]

drawPaytable :: Picture
drawPaytable =
    pictures
        [ drawLabel 230 195 0.17 (makeColorI 255 235 172 255) "Paytable"
        , drawLabel 230 150 0.105 white "777 pays 20x"
        , drawLabel 230 120 0.105 white "Diamonds pay 15x"
        , drawLabel 230 90 0.105 white "BAR pays 10x"
        , drawLabel 230 60 0.105 white "Bells pay 8x"
        , drawLabel 230 30 0.105 white "Any three pays 5x"
        , drawLabel 230 0 0.105 white "Two cherries pay 2x"
        , drawLabel 230 (-30) 0.105 white "One cherry pays 1x"
        ]

handleEvent :: Event -> GameState -> IO GameState
handleEvent event st =
    case event of
        EventKey (SpecialKey KeySpace) Down _ _ -> spinSlots st
        EventKey (SpecialKey KeyEnter) Down _ _ -> spinSlots st
        EventKey (SpecialKey KeyUp) Down _ _ -> pure $ changeBet 1 st
        EventKey (SpecialKey KeyDown) Down _ _ -> pure $ changeBet (-1) st
        EventKey (Char '+') Down _ _ -> pure $ changeBet 1 st
        EventKey (Char '-') Down _ _ -> pure $ changeBet (-1) st
        EventKey (Char 'm') Down _ _ -> pure $ maxBet st
        EventKey (Char 'M') Down _ _ -> pure $ maxBet st
        EventKey (Char 'r') Down _ _ -> pure resetSlots
        EventKey (Char 'R') Down _ _ -> pure resetSlots
        EventKey (MouseButton LeftButton) Down _ mousePos ->
            if spinButtonClicked mousePos
                then spinSlots st
                else pure st
        _ -> pure st

stepWorld :: Float -> GameState -> IO GameState
stepWorld _ st = pure st

spinButtonClicked :: (Float, Float) -> Bool
spinButtonClicked (mx, my) =
    let dx = mx - 155
        dy = my + 230
     in dx * dx + dy * dy <= 44 * 44

drawLabel :: Float -> Float -> Float -> Color -> String -> Picture
drawLabel x y size labelColor label =
    pictures
        [ translate (x + 2) (y - 2) $ scale size size $ color (makeColorI 18 18 20 180) $ text label
        , translate x y $ scale size size $ color labelColor $ text label
        ]

trimMessage :: Int -> String -> String
trimMessage maxLen msg
    | length msg <= maxLen = msg
    | maxLen <= 3 = take maxLen msg
    | otherwise = take (maxLen - 3) msg ++ "..."
