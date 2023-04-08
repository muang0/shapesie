# Shapesie
⬛👀

A library for solving shape placement puzzles.  Inspired by a daily [puzzle](https://www.amazon.com/DragonFjord-Puzzle-Day-Original-Challenges/dp/B09BHV12QF) recommended to me by a friend

## Approach
🌳🕵️‍♀️

Given a two-dimensional board and pieces, attempt to place all pieces onto the board in a non-overlapping manner.  Each placement round, search for the 'most restricted' spot on the board and try to find a piece that matches the spot.  Continue until all pieces have been placed, or the search space has been exhausted.
