module BalatroMini.Shop
  ( openShop
  , leaveShop
  , buyShopItem
  , moveShopCursor
  , removeDeckCard
  , jokerOfferWeight
  , randomJokerOffers
  , randomizeJokerEnhancement
  , enhancedCardOfferPool
  , enhancedCardPrice
  , randomEnhancedCardOffers
  ) where

import System.Random (RandomGen, randomR)

import BalatroMini.Cards (enhancedEnchantments, jokerEnhancedEnchantments, showPlayingCard)
import BalatroMini.Content (handTypeName, jokerPool, upgradeHandPool)
import BalatroMini.Run (startBlind)
import BalatroMini.Scoring (handUpgradeLevel, upgradeHand)
import BalatroMini.Types
import BalatroMini.Utils (removeAt, shuffleList)

openShop :: Game -> Game
openShop game =
  let availableJokers = filter (\joker -> all ((/= jokerId joker) . jokerId) (jokers game)) jokerPool
      (jokerOffers, genJokers) = randomJokerOffers (jokerOfferWeight availableJokers) (generator game)
      (cardOffers, genCards) = randomEnhancedCardOffers 5 genJokers
      upgradeOffers = map (`ShopUpgrade` 5) upgradeHandPool
      offerPool = jokerOffers ++ cardOffers ++ upgradeOffers
      (shuffledOffers, gen1) = shuffleList offerPool genCards
   in game
        { phase = Shopping
        , shopItems = take 5 shuffledOffers
        , removalsLeft = 0
        , shopDeckCursor = 0
        , message = "Shop: buy with 1-5, Enter starts the next blind."
        , generator = gen1
        }

leaveShop :: Game -> Game
leaveShop game =
  startBlind $
    game
      { phase = PlayingBlind
      , shopItems = []
      , message = "New blind started."
      }

buyShopItem :: Int -> Game -> Game
buyShopItem idx game
  | idx >= length (shopItems game) = game
  | otherwise =
      case shopItems game !! idx of
        ShopJoker joker
          | money game < jokerCost joker ->
              game {message = "Not enough cash for that joker."}
          | otherwise ->
              game
                { money = money game - jokerCost joker
                , jokers = jokers game ++ [joker]
                , shopItems = removeAt idx (shopItems game)
                , message = jokerName joker ++ " added to your joker tray."
                }
        ShopCard card price
          | money game < price ->
              game {message = "Not enough cash for that card."}
          | otherwise ->
              game
                { money = money game - price
                , permanentDeck = permanentDeck game ++ [card]
                , shopItems = removeAt idx (shopItems game)
                , message = showPlayingCard card ++ " joined your deck."
                }
        ShopUpgrade handKind price
          | money game < price ->
              game {message = "Not enough cash for that upgrade."}
          | otherwise ->
              let nextLevel = handUpgradeLevel handKind game + 1
               in game
                    { money = money game - price
                    , handUpgrades = upgradeHand handKind (handUpgrades game)
                    , shopItems = removeAt idx (shopItems game)
                    , message = handTypeName handKind ++ " upgraded to Lv " ++ show nextLevel ++ "."
                    }

moveShopCursor :: Int -> Game -> Game
moveShopCursor delta game
  | null (permanentDeck game) = game
  | otherwise =
      let cap = length (permanentDeck game) - 1
          nextCursor = max 0 (min cap (shopDeckCursor game + delta))
       in game {shopDeckCursor = nextCursor}

removeDeckCard :: Game -> Game
removeDeckCard game
  | removalsLeft game <= 0 = game {message = "You already removed 3 cards in this shop."}
  | null (permanentDeck game) = game
  | length (permanentDeck game) <= 5 = game {message = "Deck is already lean enough."}
  | otherwise =
      let idx = shopDeckCursor game
          removedCard = permanentDeck game !! idx
          nextDeck = removeAt idx (permanentDeck game)
          nextCursor = max 0 (min (length nextDeck - 1) idx)
       in game
            { permanentDeck = nextDeck
            , removalsLeft = removalsLeft game - 1
            , shopDeckCursor = nextCursor
            , message = "Removed " ++ showPlayingCard removedCard ++ " from your deck."
            }

jokerOfferWeight :: [Joker] -> [Joker]
jokerOfferWeight availableJokers =
  availableJokers ++ availableJokers

randomJokerOffers :: RandomGen g => [Joker] -> g -> ([ShopItem], g)
randomJokerOffers [] gen = ([], gen)
randomJokerOffers (joker : rest) gen =
  let (offeredJoker, gen1) = randomizeJokerEnhancement joker gen
      (restOffers, gen2) = randomJokerOffers rest gen1
   in (ShopJoker offeredJoker : restOffers, gen2)

randomizeJokerEnhancement :: RandomGen g => Joker -> g -> (Joker, g)
randomizeJokerEnhancement joker gen =
  let (roll, gen1) = randomR (1 :: Int, 5) gen
   in if roll == 1
        then
          let (idx, gen2) = randomR (0, length jokerEnhancedEnchantments - 1) gen1
           in (joker {jokerEnchant = jokerEnhancedEnchantments !! idx}, gen2)
        else (joker {jokerEnchant = NoEnchant}, gen1)

enhancedCardOfferPool :: [ShopItem]
enhancedCardOfferPool =
  [ ShopCard (PlayingCard rank suit enchantment) (enhancedCardPrice enchantment)
  | rank <- [minBound .. maxBound]
  , suit <- [minBound .. maxBound]
  , enchantment <- enhancedEnchantments
  ]

enhancedCardPrice :: Enchant -> Int
enhancedCardPrice enchantment =
  case enchantment of
    BonusChips -> 4
    LuckyMult -> 4
    TimesMult -> 6
    GoldSeal -> 5
    WildSuit -> 5
    NoEnchant -> 3

randomEnhancedCardOffers :: RandomGen g => Int -> g -> ([ShopItem], g)
randomEnhancedCardOffers count gen =
  let (shuffledOffers, nextGen) = shuffleList enhancedCardOfferPool gen
   in (take count shuffledOffers, nextGen)
