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
