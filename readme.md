# WIP

This project is intended to solve a daily [puzzle](https://www.amazon.com/DragonFjord-Puzzle-Day-Original-Challenges/dp/B09BHV12QF) recommended to me by a friend

## Approach

DFS 🌳🕵️‍♀️

## Data Types
- Board: 2D array where each element represents a board space.  Each space encodes the following information: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}.  
    - piece_used: Identifier for which piece is placed in a given space.  In practice, the piece_used field will be extended to 13 bits to make full use of Rusts u16 primitive (we need at least 3 bits for the 8-piece puzzle linked above; although in theory we could get away with only 2 bits depending on how strict we want to be regarding final board representation).
    - is_board: Is this space on the board or out of bounds?
    - is_open: Is this space open or occupied by a piece?
    - neighbors: Each bit in this field represents if a neighbor exists on a particular side of the space (a neighbor could be a placed piece or the edge of the board)  ☝️👉👇👈
- Pieces: 4D array that holds all possible rotations for all pieces.  The 1st dimension iterates across each unique board piece, and the 2nd dimension iterates across all unique rotations of a given piece.  The 3rd & 4th dimension are used for encoding the following information about each piece rotation: {1b': is_piece, 4b': neighbors}.
    - is_piece: does this space represent a piece or empty space?
    - neighbors: if is_piece {Each bit in this field represents if an **open space** exists on a particular side of the space ☝️👉👇👈} else {Each bit in this field represents if a **board piece** exists on a particular side of the space ☝️👉👇👈}
