module Main where

import qualified BalatroMini as Balatro
import qualified Blackjack as Blackjack
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Roulette.UI as Roulette
import qualified Roulette.Types as Roulette
import qualified Slots.Types as Slots
import qualified Slots.UI as Slots

data CasinoState = CasinoState
    { screen :: CasinoScreen
    , sharedBankroll :: Double
    , menuTapCount :: Int
    , menuTapTimer :: Float
    }

data CasinoScreen
    = Menu
    | BlackjackScreen Blackjack.Game
    | BalatroScreen Balatro.Game
    | RouletteScreen Roulette.GameState
    | SlotsScreen Slots.GameState

main :: IO ()
main =
    playIO
        (InWindow "Haskell Casino" (1280, 860) (100, 80))
        (makeColorI 10 65 52 255)
        60
        (CasinoState Menu 500 0 0)
        renderCasino
        handleCasinoEvent
        stepCasino

renderCasino :: CasinoState -> IO Picture
renderCasino casino =
    case screen casino of
        Menu -> pure $ renderMenu (sharedBankroll casino)
        BlackjackScreen game -> pure $ Blackjack.renderGame game
        BalatroScreen game -> pure $ Balatro.renderGame game
        RouletteScreen game -> Roulette.render game
        SlotsScreen game -> Slots.render game

handleCasinoEvent :: Event -> CasinoState -> IO CasinoState
handleCasinoEvent event casino =
    case event of
        EventKey (Char 'm') Down _ _ -> pure $ handleMenuShortcut casino
        EventKey (Char 'M') Down _ _ -> pure $ handleMenuShortcut casino
        EventKey (SpecialKey KeyEsc) Down _ _ -> pure $ returnToMenu casino
        _ ->
            case screen casino of
                Menu -> handleMenuEvent event casino
                BlackjackScreen game ->
                    pure casino {screen = BlackjackScreen (Blackjack.handleEvent event game)}
                BalatroScreen game ->
                    pure casino {screen = BalatroScreen (Balatro.handleEvent event game)}
                RouletteScreen game -> do
                    nextGame <- Roulette.handleEvent event game
                    pure casino {screen = RouletteScreen nextGame}
                SlotsScreen game -> do
                    nextGame <- Slots.handleEvent event game
                    pure casino {screen = SlotsScreen nextGame}

stepCasino :: Float -> CasinoState -> IO CasinoState
stepCasino dt casino =
    case screen casino of
        Menu ->
            pure $
                casino
                    { menuTapTimer = max 0 (menuTapTimer casino - dt)
                    , menuTapCount =
                        if menuTapTimer casino <= dt
                            then 0
                            else menuTapCount casino
                    }
        BlackjackScreen game ->
            pure casino {screen = BlackjackScreen (Blackjack.stepGame dt game)}
        BalatroScreen game ->
            pure casino {screen = BalatroScreen (Balatro.stepGame dt game)}
        RouletteScreen game -> do
            nextGame <- Roulette.stepWorld dt game
            pure casino {screen = RouletteScreen nextGame}
        SlotsScreen game -> do
            nextGame <- Slots.stepWorld dt game
            pure casino {screen = SlotsScreen nextGame}

handleMenuEvent :: Event -> CasinoState -> IO CasinoState
handleMenuEvent event casino =
    case event of
        EventKey (Char '1') Down _ _ -> openBlackjack casino
        EventKey (Char 'b') Down _ _ -> openBlackjack casino
        EventKey (Char 'B') Down _ _ -> openBlackjack casino
        EventKey (Char '2') Down _ _ -> pure $ openRoulette casino
        EventKey (Char 'r') Down _ _ -> pure $ openRoulette casino
        EventKey (Char 'R') Down _ _ -> pure $ openRoulette casino
        EventKey (Char '3') Down _ _ -> pure $ openSlots casino
        EventKey (Char 's') Down _ _ -> pure $ openSlots casino
        EventKey (Char 'S') Down _ _ -> pure $ openSlots casino
        EventKey (Char '4') Down _ _ -> pure $ openBalatro casino
        EventKey (Char 'a') Down _ _ -> pure $ openBalatro casino
        EventKey (Char 'A') Down _ _ -> pure $ openBalatro casino
        _ -> pure casino

openBlackjack :: CasinoState -> IO CasinoState
openBlackjack casino = do
    game <- Blackjack.initialGameWithChips (sharedBankroll casino)
    pure casino {screen = BlackjackScreen game, menuTapCount = 0, menuTapTimer = 0}

openRoulette :: CasinoState -> CasinoState
openRoulette casino =
    casino
        { screen = RouletteScreen Roulette.initialState {Roulette.balance = round (sharedBankroll casino)}
        , menuTapCount = 0
        , menuTapTimer = 0
        }

openSlots :: CasinoState -> CasinoState
openSlots casino =
    casino
        { screen = SlotsScreen Slots.initialState {Slots.balance = round (sharedBankroll casino)}
        , menuTapCount = 0
        , menuTapTimer = 0
        }

openBalatro :: CasinoState -> CasinoState
openBalatro casino =
    let seed = 4242 + round (sharedBankroll casino * 10)
     in casino
            { screen = BalatroScreen (Balatro.startFreshGame seed)
            , menuTapCount = 0
            , menuTapTimer = 0
            }

returnToMenu :: CasinoState -> CasinoState
returnToMenu casino =
    casino
        { screen = Menu
        , sharedBankroll = currentBankroll casino
        , menuTapCount = 0
        , menuTapTimer = 0
        }

handleMenuShortcut :: CasinoState -> CasinoState
handleMenuShortcut casino =
    case screen casino of
        Menu -> triggerRefillTap casino
        _ -> returnToMenu casino

triggerRefillTap :: CasinoState -> CasinoState
triggerRefillTap casino =
    let nextCount =
            if menuTapTimer casino > 0
                then menuTapCount casino + 1
                else 1
     in if nextCount >= 4
            then
                casino
                    { sharedBankroll = sharedBankroll casino + 1000
                    , menuTapCount = 0
                    , menuTapTimer = 0
                    }
            else
                casino
                    { menuTapCount = nextCount
                    , menuTapTimer = 1.2
                    }

currentBankroll :: CasinoState -> Double
currentBankroll casino =
    case screen casino of
        Menu -> sharedBankroll casino
        BlackjackScreen game -> Blackjack.blackjackBankroll game
        BalatroScreen game -> sharedBankroll casino + Balatro.balatroCasinoPayout game
        RouletteScreen game -> fromIntegral (Roulette.balance game)
        SlotsScreen game -> fromIntegral (Slots.balance game)

renderMenu :: Double -> Picture
renderMenu bankroll =
    pictures
        [ color (makeColorI 10 65 52 255) $ rectangleSolid 1280 860
        , color (makeColorI 78 38 52 255) $ translate 0 20 $ rectangleSolid 760 620
        , color (makeColorI 232 187 82 255) $ translate 0 20 $ rectangleWire 760 620
        , label (-205) 230 0.45 (makeColorI 255 235 172 255) "Haskell Casino"
        , label (-138) 175 0.17 white ("Shared bankroll: $" ++ formatMoney bankroll)
        , menuButton 105 "1" "Blackjack" "Cards, splits, double downs"
        , menuButton (-5) "2" "Roulette" "Chips, table bets, spinning wheel"
        , menuButton (-115) "3" "Slots" "Pull the lever or press spin"
        , menuButton (-225) "4" "Balancho" ("Starts with $" ++ show Balatro.balatroStartingMoney ++ ", only profit cashes out")
        , label (-250) (-315) 0.13 white "Press 1, 2, 3, or 4. Press M or Esc inside any game to come back here."
        ]

menuButton :: Float -> String -> String -> String -> Picture
menuButton y key title subtitle =
    pictures
        [ color (makeColorI 34 89 78 255) $ translate 0 y $ rectangleSolid 520 82
        , color (makeColorI 232 187 82 255) $ translate 0 y $ rectangleWire 520 82
        , label (-222) (y + 8) 0.22 (makeColorI 255 235 172 255) (key ++ ". " ++ title)
        , label (-222) (y - 22) 0.115 white subtitle
        ]

label :: Float -> Float -> Float -> Color -> String -> Picture
label x y size labelColor value =
    pictures
        [ translate (x + 2) (y - 2) $ scale size size $ color (makeColorI 15 15 18 180) $ text value
        , translate x y $ scale size size $ color labelColor $ text value
        ]

formatMoney :: Double -> String
formatMoney amount =
    if abs (amount - fromIntegral (round amount :: Int)) < 0.001
        then show (round amount :: Int)
        else show amount
