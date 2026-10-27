### Haskell Casino

This is a project that me and a partner worked together on that simulates a mini Casino in Haskell using a GUI. There are 4 games to play: Blackjack, Slots, Roulette, and a Mini Balatro game. When you first run the game you will load into a screen that has all 4 games and a short description of each game. You press the corresponding number on your keyboard to the game that you want to play, then you get loaded into the game. You start the game with $500 and that money is shared between Blackjack, Slots, and Roulette (Mini Balatro uses its own money). Then you can play to your hearts content or until you lose all your money. Each game also has short descriptions on how to play the game, and for Balatro, it plays pretty much exactly like base Balatro.

## Program Internals

- `src/Roulette/Types.hs` keeps the shared data model together.
- `src/Roulette/Logic.hs` contains pure roulette rules and balance updates.
- `src/Roulette/UI.hs` uses `Gloss` to draw. 

- `src/Slots/Types.hs` defines the reel symbols, spin results, bet steps, and game state.
- `src/Slots/Logic.hs` contains the slot rules: weighted reel spins, payout multipliers, and bet changes.
- `src/Slots/UI.hs` uses `Gloss` to draw the machine and lever and handles input.

- `src/Blackjack/` holds Blackjack, split into `Types`, `Cards` (deck and hand totals), `State` (game-state helpers), `Actions` (hit/stand/split/double/insurance/surrender), `Settlement` (dealer play and payouts), `Input`, `Render`, `Chips`, and `Utils`. `Blackjack.Game` is the public entry point used by `Main`.

- `src/BalatroMini/` holds Balatro Mini, split into `Types`, `Cards`, `Content` (blinds, jokers, hand tables), `HandEval` (poker hand detection), `Jokers`, `Scoring`, `Economy` (cash and interest), `Run` (starting runs and blinds), `Round` (playing and discarding), `Shop`, `Layout` (click areas), `Input`, `Render` (with `Render/Widgets` and `Render/Overlays`), and `Utils`. `BalatroMini.Game` is the public entry point used by `Main`.

- `app/Main.hs` is just the app entry point.

## Running

First change dir into "Haskell Games", then if you have Cabal installed:

```bash
cabal run casino-games
```

## Notes

- Ignore `src - blackjack`, that folder has older versions of one of the games.
- There currently isn't any plans to add any new games, but I would consider adding one just to make it 5 total games, maybe some like pack opening game.
- The UI in Mini Balatro can be a little jank, for example when in a shop the text on some of the cards is too small to read, probably should try and fix that. 
