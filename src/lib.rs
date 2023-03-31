use std::fmt;

// TODO support non-uniform vector size piece inputs
//  ((1, 1, 0),
//   (1))
// TODO support more than 8 pieces input
// TODO refactor neighbor calc fn (DRY)
// TODO refactor how target space on board is found (DRY)
// TODO implement board/piece public interface
// TODO support puzzles where unfilled spaces in solution is acceptable
// TODO abstract calculate_neighbors functions to handle either board or piece input

#[derive(Debug)]
pub enum Error {
    NoBoardTargetSpaceFound,
    NoSolutionFound,
    PiecePlacementBounds,
    TooManyPieces,
    BoardTooTall,
    BoardTooWide,
    PieceTooTall,
    PieceTooWide,
}

pub struct Board(Vec<Vec<u16>>);

// board serialization: {3b': uid, 1b': is_board, 1'b: is_open, 4b': neighbors}
impl fmt::Binary for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        println!();
        for x_arr in self.0.iter() {
            for piece in x_arr.iter() {
                let uid = piece >> 6 & 0b_111;
                let is_board = piece >> 5 & 0b_1;
                let is_open = piece >> 4 & 0b_1;
                let neighbors = piece & 0b_1111;
                write!(
                    f,
                    "0b_{:03b}_{:01b}_{:01b}_{:04b} ",
                    uid, is_board, is_open, neighbors
                )?;
            }
            println!()
        }
        Ok(())
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        println!();
        for x_arr in self.0.iter() {
            for piece in x_arr.iter() {
                let uid = piece >> 6 & 0b_111;
                let is_board = piece >> 5 & 0b_1;
                if is_board == 1 {
                    write!(f, "{} ", uid)?;
                } else {
                    write!(f, "  ")?;
                }
            }
            println!()
        }
        Ok(())
    }
}

impl Board {
    fn new(board: &Vec<Vec<bool>>) -> Self {
        let mut board = Board(
            board
                .iter()
                .map(|x_arr| {
                    x_arr
                        .iter()
                        .map(|square| {
                            if *square {
                                0b_000_1_1_0000
                            } else {
                                0b_000_0_0_0000
                            }
                        })
                        .collect()
                })
                .collect(),
        );
        calculate_neighbors_board(&mut board);
        board
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Piece(Vec<Vec<u8>>);

// piece serialization : {1b': is_piece, 4b': neighbors}
impl fmt::Binary for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        println!();
        for x_arr in self.0.iter() {
            for piece in x_arr.iter() {
                let is_piece = piece >> 4 & 0b_1;
                let neighbors = piece & 0b_1111;
                write!(f, "0b_{:01b}_{:04b} ", is_piece, neighbors)?;
            }
            println!()
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
struct PiecePermuted {
    uid: u8,                    // used for identifying the piece once placed on the board
    piece_permuted: Vec<Piece>, // all possible unique orientations of the piece
}

// solve the puzzle by attempting to place all pieces onto the board (non-overlapping)
pub fn solve_puzzle(board: &Vec<Vec<bool>>, pieces: &Vec<Vec<Vec<bool>>>) -> Result<Board, Error> {
    validate_input(board, pieces)?;
    let pieces_permuted = permute_pieces(pieces)?;
    let board = Board::new(board);
    return attempt_move(board, pieces_permuted, true);
}

// validates input constraints
fn validate_input(board: &Vec<Vec<bool>>, pieces: &Vec<Vec<Vec<bool>>>) -> Result<(), Error> {
    let board_width_bound = (usize::MAX / 2) - 1;
    if board.len() >= board_width_bound {
        return Err(Error::BoardTooTall);
    }
    for x_arr in board.iter() {
        if x_arr.len() >= board_width_bound {
            return Err(Error::BoardTooWide);
        }
    }
    for piece in pieces.iter() {
        let piece_width_bound = (usize::MAX / 2) - 1;
        if piece.len() >= piece_width_bound {
            return Err(Error::PieceTooTall);
        }
        for x_arr in piece.iter() {
            if x_arr.len() >= piece_width_bound {
                return Err(Error::PieceTooWide);
            }
        }
    }
    Ok(())
}

// find all possible orientations of the provided pieces
fn permute_pieces(pieces: &Vec<Vec<Vec<bool>>>) -> Result<Vec<PiecePermuted>, Error> {
    let mut pieces_permuted: Vec<PiecePermuted> = Vec::new();
    // loop through pieces
    for (index, piece) in pieces.iter().enumerate() {
        // rotate & encode piece 4x
        let uid_result = u8::try_from(index);
        if uid_result.is_err() {
            return Err(Error::TooManyPieces);
        }
        let mut piece_permuted = PiecePermuted {
            uid: uid_result.unwrap(),
            piece_permuted: Vec::new(),
        };
        let mut rotated_piece = piece.clone();
        piece_permuted
            .piece_permuted
            .push(encode_piece(&rotated_piece));
        for _ in 0..3 {
            rotated_piece = rotate_piece(&rotated_piece);
            piece_permuted
                .piece_permuted
                .push(encode_piece(&rotated_piece))
        }
        // flip piece, then rotate & encode 4x
        let mut flipped_rotated_piece = flip_piece(&piece.clone());
        piece_permuted
            .piece_permuted
            .push(encode_piece(&flipped_rotated_piece));
        for _ in 0..3 {
            flipped_rotated_piece = rotate_piece(&flipped_rotated_piece);
            piece_permuted
                .piece_permuted
                .push(encode_piece(&flipped_rotated_piece))
        }
        // remove duplicate piece permutations
        piece_permuted.piece_permuted.sort();
        piece_permuted.piece_permuted.dedup();
        pieces_permuted.push(piece_permuted)
    }
    return Ok(pieces_permuted);
}

// piece serialization : {1b': is_piece, 4b': neighbors}
fn encode_piece(piece: &Vec<Vec<bool>>) -> Piece {
    let mut encoded_piece = Piece(
        piece
            .iter()
            .map(|x_arr| {
                x_arr
                    .iter()
                    .map(|is_piece| {
                        if *is_piece {
                            return 0b_1_0000;
                        } else {
                            return 0b_0_0000;
                        }
                    })
                    .collect()
            })
            .collect(),
    );
    calculate_neighbors_piece(&mut encoded_piece);
    return encoded_piece;
}

// copies and rotates a piece (90 degrees clockwise)
fn rotate_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let mut rotated_piece = vec![vec![false; piece.len()]; piece[0].len()];
    for (y_index, x_vec) in piece.iter().rev().enumerate() {
        for (x_index, val) in x_vec.iter().enumerate() {
            rotated_piece[x_index][y_index] = val.clone();
        }
    }
    return rotated_piece;
}

// copies and flips a piece
fn flip_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    return piece.iter().rev().map(|y| y.clone()).collect();
}

// calculates 'neighbors' serialized field for each square on the board
// each bit in this field represents if a neighbor exists on a particular side of the space
// a neighbor could be a placed piece or the edge of the board  ☝️👉👇👈
fn calculate_neighbors_board(board: &mut Board) {
    let board_reference = board.0.clone();
    for (y_index, x_arr) in board.0.iter_mut().enumerate() {
        for (x_index, square) in x_arr.iter_mut().enumerate() {
            // don't need to calculate neighbors for off-board squares
            if *square | 0b_111_0_1_1111 == 0b_111_1_1_1111 {
                let y_index_signed = y_index as isize; // already validated max board input width above
                let x_index_signed = x_index as isize;
                let offsets: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
                for (offset_index, (y_offset, x_offset)) in offsets.iter().enumerate() {
                    let neighbor_open_encoding =
                        (0b_111_0111 >> offset_index) | 0b_1111_1111_1111_0000;
                    let neighbor_closed_encoding = !neighbor_open_encoding;
                    if let Some(x_arr2) = board_reference.get((y_index_signed + *y_offset) as usize)
                    {
                        if let Some(neighbor) = x_arr2.get((x_index_signed + *x_offset) as usize) {
                            if neighbor & 0b_1_1_0000 != 0b_1_1_0000 {
                                *square = *square | neighbor_closed_encoding; // neighbor is off the board or blocked
                            } else {
                                *square = *square & neighbor_open_encoding; // neighbor is a valid, open space
                            }
                        } else {
                            *square = *square | neighbor_closed_encoding; // neighbor is off the board
                        }
                    } else {
                        *square = *square | neighbor_closed_encoding; // neighbor is off the board
                    }
                }
            }
        }
    }
}

// calculates 'neighbors' serialized field for each square on the piece vector
// if the square represents a piece => Each bit in this field represents if an **open space** exists on a particular side of the space ☝️👉👇👈
fn calculate_neighbors_piece(piece: &mut Piece) {
    let piece_reference = piece.0.clone();
    for (y_index, x_arr) in piece.0.iter_mut().enumerate() {
        for (x_index, square) in x_arr.iter_mut().enumerate() {
            // don't need to calculate neighbors for off-board squares
            if *square & 0b_1_0000 == 0b_1_0000 {
                let y_index_signed = y_index as isize; // already validated max piece input width above
                let x_index_signed = x_index as isize;
                let offsets: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
                for (offset_index, (y_offset, x_offset)) in offsets.iter().enumerate() {
                    let neighbor_open_encoding = (0b_1111_0111 >> offset_index) | 0b_1111_0000;
                    let neighbor_closed_encoding = !neighbor_open_encoding;
                    if let Some(x_arr2) = piece_reference.get((y_index_signed + *y_offset) as usize)
                    {
                        if let Some(neighbor) = x_arr2.get((x_index_signed + *x_offset) as usize) {
                            if neighbor & 0b_1_0000 != 0b_1_0000 {
                                *square = *square | neighbor_closed_encoding; // neighbor is not a piece
                            } else {
                                *square = *square & neighbor_open_encoding; // neighbor is a piece
                            }
                        } else {
                            *square = *square | neighbor_closed_encoding; // neighbor is not a piece
                        }
                    } else {
                        *square = *square | neighbor_closed_encoding; // neighbor is not a piece
                    }
                }
            }
        }
    }
}

// recursively attempt piece placements until solution is found or search space has been exhausted
fn attempt_move(
    board: Board,
    pieces_permuted: Vec<PiecePermuted>,
    should_recurse: bool,
) -> Result<Board, Error> {
    // find first 'most restricted' space
    // 1st 3-walled space
    let mut target_space = board.0.iter().enumerate().find_map(|(y_index, x_vec)| {
        x_vec.iter().enumerate().find_map(|(x_index, space)| {
            if space & 0b_1_1_0111 == 0b_1_1_0111
                || space & 0b1_1_1011 == 0b_1_1_1011
                || space & 0b1_1_1101 == 0b_1_1_1101
                || space & 0b1_1_1110 == 0b_1_1_1110
            {
                return Some((y_index, x_index));
            } else {
                return None;
            }
        })
    }); // TODO refactor this pattern of finding the 'most restricted' space (for loop + input of num_walls)

    // if no 3-walled space found, search for fist corner
    if target_space == None {
        target_space = board.0.iter().enumerate().find_map(|(y_index, x_vec)| {
            x_vec.iter().enumerate().find_map(|(x_index, space)| {
                if space & 0b_1_1_0011 == 0b_1_1_0011
                    || space & 0b_1_1_1001 == 0b_1_1_1001
                    || space & 0b_1_1_1100 == 0b_1_1_1100
                    || space & 0b_1_1_0110 == 0b_1_1_0110
                {
                    return Some((y_index, x_index));
                } else {
                    return None;
                }
            })
        });
    }

    // find a piece that has a valid anchor point (⚓️ the piece onto the board by finding a square on the piece that fits the target space without any neighbor conflicts)
    if let Some((y_index_board, x_index_board)) = target_space {
        // for each piece
        for (piece_index, piece_permuted) in pieces_permuted.iter().enumerate() {
            // for each possible orientation of a given piece
            for piece in piece_permuted.piece_permuted.iter() {
                // 👀 through the piece squares for an ⚓️ point
                for (y_index_piece_anchor, x_arr) in piece.0.iter().enumerate() {
                    for (x_index_piece_anchor, piece_space_anchor) in x_arr.iter().enumerate() {
                        // piece ⚓️ should fit onto the board's target space without any neighbor/wall conflicts
                        if piece_space_anchor & 0b_1_0000 == 0b_1_0000
                            && u16::from(*piece_space_anchor)
                                & (board.0[y_index_board][x_index_board] & 0b_1111)
                                == (board.0[y_index_board][x_index_board] & 0b_1111)
                        {
                            // determine if piece is a valid move (given fixed board target space & piece ⚓️)
                            let piece_is_blocked = piece.0.iter().enumerate().any(|(y_index_piece, x_arr2)| x_arr2.iter().enumerate().any(|(x_index_piece, piece_space)|
                                // check if piece vector item corresponds to a piece square
                                if piece_space & 0b_1_0000 == 0b_1_0000 {
                                    // check if associated board square is open
                                    let target_y_index_opt = (y_index_board + y_index_piece).checked_sub(y_index_piece_anchor);
                                    if let Some(target_y_index) = target_y_index_opt {
                                        if let Some(x_arr2) = board.0.get(target_y_index) {
                                            let target_x_index_opt = (x_index_board + x_index_piece).checked_sub(x_index_piece_anchor);
                                            if let Some(target_x_index) = target_x_index_opt {
                                                if let Some(board_square) = x_arr2.get(target_x_index) {
                                                    // board vector item should be 'on the board' & open to constitute a valid overlay
                                                    if board_square & 0b_1_1_0000 != 0b_1_1_0000 {
                                                        return true
                                                    } else { return false }
                                                } else { return true }
                                            } else { return true }
                                        } else { return true }
                                    } else { return true }
                                } else { return false }  // piece vector item is an empty space, which is always a valid overlay
                            ));
                            // if the piece fits, update the board accordingly
                            if !piece_is_blocked {
                                let mut updated_board = Board(board.0.clone());
                                for (y_index_piece, x_arr2) in piece.0.iter().enumerate() {
                                    for (x_index_piece, piece_space) in x_arr2.iter().enumerate() {
                                        if piece_space & 0b_1_0000 == 0b_1_0000 {
                                            // don't have to worry about subraction overflow here since already checked above
                                            if let Some(x_arr3) = updated_board.0.get_mut(
                                                y_index_board + y_index_piece
                                                    - y_index_piece_anchor,
                                            ) {
                                                if let Some(board_square) = x_arr3.get_mut(
                                                    x_index_board + x_index_piece
                                                        - x_index_piece_anchor,
                                                ) {
                                                    // serialize board square as taken by given piece uid
                                                    *board_square = 0b_1_0_0000
                                                        | u16::from(piece_permuted.uid) << 6
                                                } else {
                                                    return Err(Error::PiecePlacementBounds);
                                                }
                                            } else {
                                                return Err(Error::PiecePlacementBounds);
                                            }
                                        }
                                    }
                                }
                                // update neighbors serialization after placing the piece
                                calculate_neighbors_board(&mut updated_board);
                                let mut updated_pieces = pieces_permuted.clone();
                                updated_pieces.remove(piece_index);
                                // return updated board if all pieces have been used (or if recursion is disabled)
                                if updated_pieces.len() == 0 || !should_recurse {
                                    return Ok(updated_board);
                                }
                                // recursively attempt moves
                                let move_res = attempt_move(updated_board, updated_pieces, true);
                                // return if puzzle has been solved
                                if move_res.is_ok() {
                                    return move_res;
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        return Err(Error::NoBoardTargetSpaceFound);
    }
    return Err(Error::NoSolutionFound);
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Piecemeal {
        piece: Vec<Vec<bool>>,
        piece_flipped: Option<Vec<Vec<bool>>>,
        piece_rotated: Option<Vec<Vec<bool>>>,
        piece_encoded: Option<Vec<Vec<u8>>>,
        piece_permuted: Option<PiecePermuted>,
    }

    #[rustfmt::skip]
    fn calendar_pieces() -> Vec<Piecemeal> {
        vec!(
            Piecemeal {
                piece: vec!(
                    vec!(1, 1),
                    vec!(1, 0),
                    vec!(1, 0),
                    vec!(1, 0)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: Some(vec!(
                    vec!(1, 0),
                    vec!(1, 0),
                    vec!(1, 0),
                    vec!(1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect()),
                piece_rotated: Some(vec!(
                    vec!(1, 1, 1, 1),
                    vec!(0, 0, 0, 1)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect()),
                piece_encoded: Some(vec!(
                    vec!(0b_1_1001, 0b_1_1110),
                    vec!(0b_1_0101, 0b_0_0000),
                    vec!(0b_1_0101, 0b_0_0000),
                    vec!(0b_1_0111, 0b_0_0000),
                )),
                piece_permuted: Some(
                    PiecePermuted {
                        uid: 1,
                        piece_permuted: vec!(
                            Piece(vec!(
                                vec!(0b_0_0000, 0b_0_0000, 0b_0_0000, 0b_1_1101),
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_0110),
                            )),
                            Piece(vec!(
                                vec!(0b_0_0000, 0b_1_1101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_1_1011, 0b_1_0110),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1010, 0b_1_1010, 0b_1_1110),
                                vec!(0b_1_0111, 0b_0_0000, 0b_0_0000, 0b_0_0000),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0111, 0b_0_0000),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_1100),
                                vec!(0b_0_0000, 0b_0_0000, 0b_0_0000, 0b_1_0111),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1011, 0b_1_1100),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0111),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1110),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1101, 0b_0_0000, 0b_0_0000, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1010, 0b_1_1010, 0b_1_1110),
                            )),
                        ),
                    },
                ),
            },
            Piecemeal {
                piece: vec!(
                    vec!(1, 0),
                    vec!(1, 1),
                    vec!(1, 0),
                    vec!(1, 0)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            },
            Piecemeal {
                piece: vec!(
                    vec!(1, 0, 1),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: Some(vec!(
                    vec!(1, 1, 1),
                    vec!(1, 0, 1)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect()),
                piece_rotated: Some(vec!(
                    vec!(1, 1),
                    vec!(1, 0),
                    vec!(1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect()),
                piece_encoded: None,
                piece_permuted: Some(
                    PiecePermuted { 
                        uid: (2),
                        piece_permuted: (vec!(
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1010, 0b_1_1100),
                                vec!(0b_1_0111, 0b_0_0000, 0b_1_0111),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1110)
                            )),
                            Piece(vec!(
                                vec!(0b_1_1011, 0b_1_1100),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_1_1011, 0b_1_0110)
                            )),
                            Piece(vec!(
                                vec!(0b_1_1101, 0b_0_0000, 0b_1_1101),
                                vec!(0b_1_0011, 0b_1_1010, 0b_1_0110),
                            )),
                        ))
                    }
                )
            },
            Piecemeal {
                piece: vec!(
                    vec!(0, 0, 1),
                    vec!(1, 1, 1),
                    vec!(1, 0, 0),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            },
            Piecemeal {
                piece: vec!(
                    vec!(1, 1, 1),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            },
            Piecemeal {
                piece: vec!(
                    vec!(1, 0, 0),
                    vec!(1, 0, 0),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            },
            Piecemeal {
                piece: vec!(
                    vec!(0, 0, 1, 1),
                    vec!(1, 1, 1, 0),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            },
            Piecemeal {
                piece: vec!(
                    vec!(1, 1, 1),
                    vec!(0, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                piece_flipped: None,
                piece_rotated: None,
                piece_encoded: None,
                piece_permuted: None
            }
        )
    }

    #[test]
    fn flips_piece() {
        for piecemeal in calendar_pieces() {
            if let Some(piece_flipped) = piecemeal.piece_flipped {
                assert_eq!(piece_flipped, flip_piece(&piecemeal.piece));
            }
        }
    }

    #[test]
    fn rotates_piece() {
        for piecemeal in calendar_pieces() {
            if let Some(piece_rotated) = piecemeal.piece_rotated {
                assert_eq!(piece_rotated, rotate_piece(&piecemeal.piece));
            }
        }
    }

    #[test]
    fn encodes_piece() {
        for piecemeal in calendar_pieces() {
            if let Some(piece_encoded) = piecemeal.piece_encoded {
                assert_eq!(piece_encoded, encode_piece(&piecemeal.piece).0);
            }
        }
    }

    #[test]
    fn permutes_pieces() {
        // collect pieces from calendar_pieces where piece_permuted is defined
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .filter(|piecemeal| piecemeal.piece_permuted.is_some())
            .map(|piecemeal| piecemeal.piece.clone())
            .collect();

        // permute pieces
        let pieces_permuted = permute_pieces(&pieces).expect("Pieces should be permutable");

        // collect pieces_permuted from calendar_pieces
        let mut expected_pieces_permuted: Vec<PiecePermuted> = calendar_pieces()
            .iter()
            .filter(|piecemeal| piecemeal.piece_permuted.is_some())
            .map(|piecemeal| piecemeal.piece_permuted.clone().unwrap())
            .collect();

        // populate uid for expected permuted pieces
        for (index, piece) in expected_pieces_permuted.iter_mut().enumerate() {
            piece.uid = u8::try_from(index).expect("More than 256 pieces");
        }

        // validate
        for (index, piece_permuted) in pieces_permuted.iter().enumerate() {
            assert_eq!(
                *expected_pieces_permuted
                    .get(index)
                    .expect("Number of expected pieces should equal the number of permuted pieces"),
                *piece_permuted
            );
        }
    }

    #[test]
    fn places_piece() {
        // we want to test the following placement, where:
        //  o = open space, c = closed space, x = placed piece
        // o o o        o x x
        // o o c   ->   o x c
        // o o c        o x c
        // o c c        o x c

        // collect permuted pieces
        let pieces_permuted: Vec<PiecePermuted> = calendar_pieces()
            .into_iter()
            .filter(|p| p.piece_permuted.is_some())
            .map(|p| p.piece_permuted.unwrap())
            .collect();

        // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
        let board = Board(vec![
            vec![0b_000_1_1_1001, 0b_000_1_1_1000, 0b_000_1_1_1110],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0011, 0b_000_1_1_0110, 0b_000_0_1_0000],
        ]);
        let expected_board = Board(vec![
            vec![0b_000_1_1_1101, 0b_001_1_0_1110, 0b_001_1_0_1111],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_0000],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_0000],
            vec![0b_000_1_1_0111, 0b_001_1_0_1110, 0b_000_0_1_0000],
        ]);

        // attempt piece placement
        let board = attempt_move(board, pieces_permuted, false).expect("Failed to attempt move");
        assert_eq!(expected_board.0, board.0);
    }

    #[test]#[rustfmt::skip]
    fn solves_puzzle() {
        // march 26 calendar board
        let board: Vec<Vec<bool>> = vec!(
            vec!(1, 1, 0, 1, 1, 1, 0),
            vec!(1, 1, 1, 1, 1, 1, 0),
            vec!(1, 1, 1, 1, 1, 1, 1),
            vec!(1, 1, 1, 1, 1, 1, 1),
            vec!(1, 1, 1, 1, 1, 1, 1),
            vec!(1, 1, 1, 1, 0, 1, 1),
            vec!(1, 1, 1, 0, 0, 0, 0)
        ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect();
        
        let expected_board = Board(vec!(
            vec!(0b_000_1_0_1111, 0b_000_1_0_1111, 0b_000_0_0_0000, 0b_001_1_0_1111, 0b_011_1_0_1111, 0b_011_1_0_1111, 0b_000_0_0_0000),
            vec!(0b_000_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_011_1_0_1111, 0b_000_0_0_0000),
            vec!(0b_000_1_0_1111, 0b_010_1_0_1111, 0b_010_1_0_1111, 0b_010_1_0_1111, 0b_110_1_0_1111, 0b_011_1_0_1111, 0b_011_1_0_1111),
            vec!(0b_000_1_0_1111, 0b_010_1_0_1111, 0b_111_1_0_1111, 0b_010_1_0_1111, 0b_110_1_0_1111, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_111_1_0_1111, 0b_111_1_0_1111, 0b_110_1_0_1111, 0b_110_1_0_1111, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_111_1_0_1111, 0b_111_1_0_1111, 0b_110_1_0_1111, 0b_000_0_0_0000, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_101_1_0_1111, 0b_101_1_0_1111, 0b_000_0_0_0000, 0b_000_0_0_0000, 0b_000_0_0_0000, 0b_000_0_0_0000),
        ));

        // collect calendar pieces
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .map(|piecemeal| piecemeal.piece.clone())
            .collect();

        // solve puzzle
        let res = solve_puzzle(&board, &pieces).expect("Failed to solve puzzle");
        assert_eq!(expected_board.0, res.0);
    }

    #[test]
    fn solves_all_possible_days() {
        // collect calendar pieces
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .map(|piecemeal| piecemeal.piece.clone())
            .collect();

        // iterate through all possible month, day combinations
        for month in 1..=12 {
            for day in 1..=31 {
                // define board
                #[rustfmt::skip]
                let mut board: Vec<Vec<bool>> = vec![
                    vec![1, 1, 1, 1, 1, 1, 0],
                    vec![1, 1, 1, 1, 1, 1, 0],
                    vec![1, 1, 1, 1, 1, 1, 1],
                    vec![1, 1, 1, 1, 1, 1, 1],
                    vec![1, 1, 1, 1, 1, 1, 1],
                    vec![1, 1, 1, 1, 1, 1, 1],
                    vec![1, 1, 1, 0, 0, 0, 0],
                ]
                .iter()
                .map(|y| y.iter().map(|x| *x == 1).collect())
                .collect();

                // block off month square on the board
                let y_index: u8 = (month - 1) / 6;
                let x_index: u8 = (month - 1) % 6;
                let month_square: &mut bool = board
                    .get_mut(usize::from(y_index))
                    .unwrap()
                    .get_mut(usize::from(x_index))
                    .unwrap();
                *month_square = false;

                // block off day square on the board
                let y_index: u8 = ((day - 1) / 7) + 2;
                let x_index: u8 = (day - 1) % 7;
                let day_square: &mut bool = board
                    .get_mut(usize::from(y_index))
                    .unwrap()
                    .get_mut(usize::from(x_index))
                    .unwrap();
                *day_square = false;

                // solve puzzle
                solve_puzzle(&board, &pieces).expect("Unable to solve puzzle");
            }
        }
    }
}
