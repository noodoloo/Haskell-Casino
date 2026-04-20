# Haskell Roulette Starter

This repo is a starter scaffold for the roulette section of a larger casino-games project.

## Why this structure

- `src/Roulette/Types.hs` keeps the shared data model together.
- `src/Roulette/Logic.hs` contains pure roulette rules and balance updates.
- `src/Roulette/UI.hs` uses `Gloss` to draw a basic roulette table and respond to keys.
- `app/Main.hs` is just the app entry point.

That split makes it easier for two people to work in parallel: one person can improve game rules and payouts while the other improves the visual layer.

## Current controls

- Left click places a chip on a number or outside-bet box.
- `C` cycles chip values through `$5`, `$10`, `$25`, and `$50`.
- `Enter` spins the wheel and resolves every placed chip together.

## Suggested next steps

1. Support more roulette bet types like dozens, columns, splits, and corners.
2. Add chip removal, undo, or clear-table controls before a spin.
3. Add tests for payout rules in `Roulette.Logic`.
4. Decide whether your course/project wants American roulette (`0` and `00`) or European roulette (`0` only).

## Running

If you have Cabal installed:

```bash
cabal run roulette-app
```
