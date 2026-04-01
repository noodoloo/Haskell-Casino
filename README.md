# Haskell Roulette Starter

This repo is a starter scaffold for the roulette section of a larger casino-games project.

## Why this structure

- `src/Roulette/Types.hs` keeps the shared data model together.
- `src/Roulette/Logic.hs` contains pure roulette rules and balance updates.
- `src/Roulette/UI.hs` uses `Gloss` to draw a basic roulette table and respond to keys.
- `app/Main.hs` is just the app entry point.

That split makes it easier for two people to work in parallel: one person can improve game rules and payouts while the other improves the visual layer.

## Current controls

- `1` places a red bet for `$25`
- `2` places a black bet for `$25`
- `3` places an even bet for `$25`
- `4` places an odd bet for `$25`
- `5` places a low bet for `$25`
- `6` places a high bet for `$25`
- `7` places a straight-up bet on `17` for `$10`
- `Enter` spins the wheel

## Suggested next steps

1. Add chip selection instead of fixed bet sizes.
2. Support more roulette bet types like dozens, columns, and splits.
3. Animate the wheel and ball rather than instantly revealing the result.
4. Add tests for payout rules in `Roulette.Logic`.
5. Decide whether your course/project wants American roulette (`0` and `00`) or European roulette (`0` only).

## Running

If you have Cabal installed:

```bash
cabal run roulette-app
```
