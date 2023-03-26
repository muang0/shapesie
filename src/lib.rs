use std::fmt;

// TODO support non-uniform vector size piece inputs
//  ((1, 1, 0),
//   (1))
// TODO support more than 8 pieces input
// TODO implement board/piece public interface
// TODO support puzzles where unfilled spaces in solution is acceptable

#[derive(Debug)]
pub struct Error;

struct Board(Vec<Vec<u16>>);

impl fmt::Binary for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        println!();
        for x_arr in self.0.iter() {
            for piece in x_arr.iter() {
                let uid = piece >> 6 & 0b_111;
                let is_board = piece >> 5 & 0b_1;
                let is_open = piece >> 4 & 0b_1;
                let neighbors = piece & 0b_1111;
                print!(
                    "0b_{:03b}_{:01b}_{:01b}_{:04b} ",
                    uid, is_board, is_open, neighbors
                );
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

impl fmt::Binary for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        println!();
        for x_arr in self.0.iter() {
            for piece in x_arr.iter() {
                let is_piece = piece >> 4 & 0b_1;
                let neighbors = piece & 0b_1111;
                print!("0b_{:01b}_{:04b} ", is_piece, neighbors);
            }
            println!()
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
struct PiecePermuted {
    uid: u8,
    piece_permuted: Vec<Piece>,
}

fn solve_puzzle(
    board: &Vec<Vec<bool>>,
    pieces: &Vec<Vec<Vec<bool>>>,
    should_recurse: bool,
) -> Result<Board, Error> {
    let pieces_permuted = permute_pieces(pieces).expect("Failed to permute pieces");
    let board = Board::new(board);
    return attempt_move(board, pieces_permuted, should_recurse);
}

fn permute_pieces(pieces: &Vec<Vec<Vec<bool>>>) -> Result<Vec<PiecePermuted>, Error> {
    let mut pieces_permuted: Vec<PiecePermuted> = Vec::new();
    // loop through pieces
    for (index, piece) in pieces.iter().enumerate() {
        // rotate & encode piece 4x
        let mut piece_permuted = PiecePermuted {
            uid: u8::try_from(index).unwrap(),
            piece_permuted: Vec::new(),
        }; // TODO safely handle try_from Result
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

fn calculate_neighbors_board(board: &mut Board) {
    // TODO refactor this mess
    let board_reference = board.0.clone(); // avoid performance penalty by using unsafe rust when updating board?
    for (y_index, x_arr) in board.0.iter_mut().enumerate() {
        for (x_index, square) in x_arr.iter_mut().enumerate() {
            // don't need to calculate neighbors for off-board squares
            if *square | 0b_111_0_1_1111 == 0b_111_1_1_1111 {
                // avoid subtraction overflow if y_index is north edge
                if y_index == 0 {
                    *square = *square | 0b_000_0_0_1000;
                } else {
                    // get north square
                    if let Some(x_arr2) = board_reference.get(y_index - 1) {
                        if let Some(north_square) = x_arr2.get(x_index) {
                            // if the north square is off the board or blocked
                            if *north_square & 0b_1_1_0000 != 0b_1_1_0000 {
                                *square = *square | 0b_000_0_0_1000;
                            } else {
                                *square = *square & 0b_111_1_1_0111; // north square is a valid, open space
                            }
                        } else {
                            *square = *square | 0b_000_0_0_1000;
                        }
                    } else {
                        *square = *square | 0b_000_0_0_1000;
                    }
                }
                // get east square
                if let Some(x_arr2) = board_reference.get(y_index) {
                    if let Some(east_square) = x_arr2.get(x_index + 1) {
                        // if the east square is off the board or blocked
                        if *east_square & 0b_1_1_0000 != 0b_1_1_0000 {
                            *square = *square | 0b_000_0_0_0100;
                        } else {
                            *square = *square & 0b_111_1_1_1011; // east square is a valid, open space
                        }
                    } else {
                        *square = *square | 0b_000_0_0_0100;
                    }
                } else {
                    *square = *square | 0b_000_0_0_0100;
                }
                // get south square
                if let Some(x_arr2) = board_reference.get(y_index + 1) {
                    if let Some(south_square) = x_arr2.get(x_index) {
                        // if the south square is off the board or blocked
                        if *south_square & 0b_1_1_0000 != 0b_1_1_0000 {
                            *square = *square | 0b_000_0_0_0010;
                        } else {
                            *square = *square & 0b_111_1_1_1101; // south square is a valid, open space
                        }
                    } else {
                        *square = *square | 0b_000_0_0_0010;
                    }
                } else {
                    *square = *square | 0b_000_0_0_0010;
                }
                // avoid subtraction overflow if x_index is on west edge
                if x_index == 0 {
                    *square = *square | 0b_000_0_0_0001;
                } else {
                    // get west square
                    if let Some(x_arr2) = board_reference.get(y_index) {
                        if let Some(west_square) = x_arr2.get(x_index - 1) {
                            // if the west square is off the board or blocked
                            if *west_square & 0b_1_1_0000 != 0b_1_1_0000 {
                                *square = *square | 0b_000_0_0_0001;
                            } else {
                                *square = *square & 0b_111_1_1_1110; // west square is a valid, open space
                            }
                        } else {
                            *square = *square | 0b_000_0_0_0001;
                        }
                    } else {
                        *square = *square | 0b_000_0_0_0001;
                    }
                }
            }
        }
    }
}

fn calculate_neighbors_piece(piece: &mut Piece) {
    // TODO refactor this mess
    let piece_reference = piece.0.clone(); // avoid performance penalty by using unsafe rust when updating piece?
    for (y_index, x_arr) in piece.0.iter_mut().enumerate() {
        for (x_index, square) in x_arr.iter_mut().enumerate() {
            // if square is part of a piece
            if *square & 0b_1_0000 == 0b_1_0000 {
                // avoid subtraction overflow if y_index is north edge
                if y_index == 0 {
                    *square = *square | 0b_1000;
                } else {
                    // get north square
                    if let Some(x_arr2) = piece_reference.get(y_index - 1) {
                        if let Some(north_square) = x_arr2.get(x_index) {
                            // if the north square is a piece
                            if *north_square & 0b_1_0000 == 0b_1_0000 {
                                *square = *square & 0b_1_0111;
                            } else {
                                *square = *square | 0b_1000; // north square is an open space
                            }
                        } else {
                            *square = *square | 0b_1000;
                        }
                    } else {
                        *square = *square | 0b_1000;
                    }
                }
                // get east square
                if let Some(x_arr2) = piece_reference.get(y_index) {
                    if let Some(east_square) = x_arr2.get(x_index + 1) {
                        // if the east square is a piece
                        if *east_square & 0b_1_0000 == 0b_1_0000 {
                            *square = *square & 0b_1_1011;
                        } else {
                            *square = *square | 0b_0100; // east square is an open space
                        }
                    } else {
                        *square = *square | 0b_0100;
                    }
                } else {
                    *square = *square | 0b_0100;
                }
                // get south square
                if let Some(x_arr2) = piece_reference.get(y_index + 1) {
                    if let Some(south_square) = x_arr2.get(x_index) {
                        // if the south square is a piece
                        if *south_square & 0b_1_0000 == 0b_1_0000 {
                            *square = *square & 0b_1_1101;
                        } else {
                            *square = *square | 0b_0010; // south square is an open space
                        }
                    } else {
                        *square = *square | 0b_0010;
                    }
                } else {
                    *square = *square | 0b_0010;
                }
                // avoid subtraction overflow if x_index is on west edge
                if x_index == 0 {
                    *square = *square | 0b_0001;
                } else {
                    // get west square
                    if let Some(x_arr2) = piece_reference.get(y_index) {
                        if let Some(west_square) = x_arr2.get(x_index - 1) {
                            // if the west square is a piece
                            if *west_square & 0b_1_0000 == 0b_1_0000 {
                                *square = *square & 0b_1_1110;
                            } else {
                                *square = *square | 0b_0001; // west square is an open space
                            }
                        } else {
                            *square = *square | 0b_0001;
                        }
                    } else {
                        *square = *square | 0b_0001;
                    }
                }
            } else {
                // square is an open space
                // avoid subtraction overflow if y_index is north edge
                if y_index == 0 {
                    *square = *square & 0b_0111;
                } else {
                    // get north square
                    if let Some(x_arr2) = piece_reference.get(y_index - 1) {
                        if let Some(north_square) = x_arr2.get(x_index) {
                            // if the north square is a piece
                            if *north_square & 0b_1_0000 == 0b_1_0000 {
                                *square = *square | 0b_1000;
                            } else {
                                *square = *square & 0b_0111; // north square is an open space
                            }
                        } else {
                            *square = *square & 0b_0111;
                        }
                    } else {
                        *square = *square & 0b_0111;
                    }
                }
                // get east square
                if let Some(x_arr2) = piece_reference.get(y_index) {
                    if let Some(east_square) = x_arr2.get(x_index + 1) {
                        // if the east square is a piece
                        if *east_square & 0b_1_0000 == 0b_1_0000 {
                            *square = *square | 0b_0100;
                        } else {
                            *square = *square & 0b_1011; // east square is an open space
                        }
                    } else {
                        *square = *square & 0b_1011;
                    }
                } else {
                    *square = *square & 0b_1011;
                }
                // get south square
                if let Some(x_arr2) = piece_reference.get(y_index + 1) {
                    if let Some(south_square) = x_arr2.get(x_index) {
                        // if the south square is a piece
                        if *south_square & 0b_1_0000 == 0b_1_0000 {
                            *square = *square | 0b_0010;
                        } else {
                            *square = *square & 0b_1101; // south square is an open space
                        }
                    } else {
                        *square = *square & 0b_1101;
                    }
                } else {
                    *square = *square & 0b_1101;
                }
                // avoid subtraction overflow if x_index is on west edge
                if x_index == 0 {
                    *square = *square & 0b_1110;
                } else {
                    // get west square
                    if let Some(x_arr2) = piece_reference.get(y_index) {
                        if let Some(west_square) = x_arr2.get(x_index - 1) {
                            // if the west square is a piece
                            if *west_square & 0b_1_0000 == 0b_1_0000 {
                                *square = *square | 0b_0001;
                            } else {
                                *square = *square & 0b_1110; // west square is an open space
                            }
                        } else {
                            *square = *square & 0b_1110;
                        }
                    } else {
                        *square = *square & 0b_1110;
                    }
                }
            }
        }
    }
}

fn attempt_move(
    board: Board,
    pieces_permuted: Vec<PiecePermuted>,
    should_recurse: bool,
) -> Result<Board, Error> {
    // find first 'most restricted' space
    // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
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

    // do any of the pieces fit over the target space
    if let Some((y_index_board, x_index_board)) = target_space {
        for (piece_index, piece_permuted) in pieces_permuted.iter().enumerate() {
            for piece in piece_permuted.piece_permuted.iter() {
                for (y_index_piece_anchor, x_arr) in piece.0.iter().enumerate() {
                    for (x_index_piece_anchor, piece_space_anchor) in x_arr.iter().enumerate() {
                        // piece anchor should fit onto the board's target space without any neighbor/wall conflicts
                        if piece_space_anchor & 0b_1_0000 == 0b_1_0000
                            && u16::from(*piece_space_anchor)
                                & (board.0[y_index_board][x_index_board] & 0b_1111)
                                == (board.0[y_index_board][x_index_board] & 0b_1111)
                        {
                            // determine if piece is a valid move (given fixed board target space & piece anchor)
                            let piece_is_blocked = piece.0.iter().enumerate().any(|(y_index_piece, x_arr2)| x_arr2.iter().enumerate().any(|(x_index_piece, piece_space)|
                                // check if piece vector item corresponds to a piece square
                                if piece_space & 0b_1_0000 == 0b_1_0000 {
                                    // check if associated board square is open
                                    let target_y_index_opt = (y_index_board + y_index_piece).checked_sub(y_index_piece_anchor);
                                    if let Some(target_y_index) = target_y_index_opt {
                                        if let Some(x_arr2) = board.0.get(target_y_index) {
                                            let target_x_index_opt = (x_index_board + x_index_piece).checked_sub(x_index_piece_anchor);
                                            if let Some(target_x_index) = target_x_index_opt {
                                                if let Some(piece) = x_arr2.get(target_x_index) {
                                                    if piece & 0b_1_1_0000 != 0b_1_1_0000 {
                                                        return true
                                                    } else { return false }
                                                } else { return true }
                                            } else { return true }
                                        } else { return true }
                                    } else { return true }
                                } else { return false }
                            ));
                            if !piece_is_blocked {
                                let mut updated_board = Board(board.0.clone());
                                // update board to reflect piece placement
                                for (y_index_piece, x_arr2) in piece.0.iter().enumerate() {
                                    for (x_index_piece, piece_space) in x_arr2.iter().enumerate() {
                                        if piece_space & 0b_1_0000 == 0b_1_0000 {
                                            // don't have to worry about subraction overflow here since checked above
                                            if let Some(x_arr3) = updated_board.0.get_mut(
                                                y_index_board + y_index_piece
                                                    - y_index_piece_anchor,
                                            ) {
                                                if let Some(piece) = x_arr3.get_mut(
                                                    x_index_board + x_index_piece
                                                        - x_index_piece_anchor,
                                                ) {
                                                    *piece = 0b_1_0_0000
                                                        | u16::from(piece_permuted.uid) << 6
                                                } else {
                                                    return Err(Error); // error getting x index of piece during placement
                                                }
                                            } else {
                                                return Err(Error); // error getting y index of piece during placement
                                            }
                                        }
                                    }
                                }
                                calculate_neighbors_board(&mut updated_board);
                                let mut updated_pieces = pieces_permuted.clone();
                                updated_pieces.remove(piece_index);
                                // return updated board if all pieces have been used
                                if updated_pieces.len() == 0 || !should_recurse {
                                    return Ok(updated_board);
                                }
                                let move_res = attempt_move(updated_board, updated_pieces, true);
                                // return result if puzzle has been solved
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
        return Err(Error); // No target space on the board was found
    }
    return Err(Error); // No solution found
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
                    vec!(0b_1_0101, 0b_0_1001),
                    vec!(0b_1_0101, 0b_0_0001),
                    vec!(0b_1_0111, 0b_0_0001),
                )),
                piece_permuted: Some(
                    PiecePermuted {
                        uid: 1,
                        piece_permuted: vec!(
                            Piece(vec!(
                                vec!(0b_0_0010, 0b_0_0010, 0b_0_0110, 0b_1_1101),
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_0110),
                            )),
                            Piece(vec!(
                                vec!(0b_00100, 0b_11101),
                                vec!(0b_00100, 0b_10101),
                                vec!(0b_00110, 0b_10101),
                                vec!(0b_11011, 0b_10110),
                            )),
                            Piece(vec!(
                                vec!(0b_11001, 0b_11010, 0b_11010, 0b_11110),
                                vec!(0b_10111, 0b_01001, 0b_01000, 0b_01000),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_1001),
                                vec!(0b_1_0101, 0b_0_0001),
                                vec!(0b_1_0111, 0b_0_0001),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_1100),
                                vec!(0b_0_1000, 0b_0_1000, 0b_0_1100, 0b_1_0111),
                            )),
                            Piece(vec!(
                                vec!(0b_11011, 0b_11100),
                                vec!(0b_01100, 0b_10101),
                                vec!(0b_00100, 0b_10101),
                                vec!(0b_00100, 0b_10111),
                            )),
                            Piece(vec!(
                                vec!(0b_11101, 0b_00001),
                                vec!(0b_10101, 0b_00001),
                                vec!(0b_10101, 0b_00011),
                                vec!(0b_10011, 0b_11110),
                            )),
                            Piece(vec!(
                                vec!(0b_11101, 0b_00011, 0b_00010, 0b_00010),
                                vec!(0b_10011, 0b_11010, 0b_11010, 0b_11110),
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
                                vec!(0b_1_0111, 0b_0_1101, 0b_1_0111),
                            )),
                            Piece(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_1011),
                                vec!(0b_1_0011, 0b_1_1110)
                            )),
                            Piece(vec!(
                                vec!(0b_1_1011, 0b_1_1100),
                                vec!(0b_0_1110, 0b_1_0101),
                                vec!(0b_1_1011, 0b_1_0110)
                            )),
                            Piece(vec!(
                                vec!(0b_1_1101, 0b_0_0111, 0b_1_1101),
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
        let pieces_permuted = permute_pieces(&pieces).unwrap(); // TODO safely handle this unwrap

        // collect pieces_permuted from calendar_pieces
        let mut expected_pieces_permuted: Vec<PiecePermuted> = calendar_pieces()
            .iter()
            .filter(|piecemeal| piecemeal.piece_permuted.is_some())
            .map(|piecemeal| piecemeal.piece_permuted.clone().unwrap())
            .collect();

        // populate uid for permuted pieces
        for (index, piece) in expected_pieces_permuted.iter_mut().enumerate() {
            piece.uid = u8::try_from(index).unwrap();
        }

        // validate
        for (index, piece_permuted) in pieces_permuted.iter().enumerate() {
            assert_eq!(
                *expected_pieces_permuted.get(index).unwrap(),
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

    #[test]
    fn solves_puzzle_tracer() {
        // test the following placement, where:
        //  o = open space, c = closed space, x = placed piece
        // o o o        x x x
        // o c c   ->   x c c
        // o c c        x c c

        // collect pieces
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .map(|piecemeal| piecemeal.piece.clone())
            .collect();

        let board: Vec<Vec<bool>> = vec![vec![1, 1, 1], vec![1, 0, 0], vec![1, 0, 0]]
            .iter()
            .map(|y| y.iter().map(|x| *x == 1).collect())
            .collect();
        let expected_board = Board(vec![
            vec![0b_101_1_0_1111, 0b_101_1_0_1111, 0b_101_1_0_1111],
            vec![0b_101_1_0_1111, 0b_000_0_0_0000, 0b_000_0_0_0000],
            vec![0b_101_1_0_1111, 0b_000_0_0_0000, 0b_000_0_0_0000],
        ]);

        // attempt puzzle solution
        let board = solve_puzzle(&board, &pieces, false).expect("Failed to solve puzzle");
        assert_eq!(expected_board.0, board.0);

        // test the following placement, where:
        //  o = open space, c = closed space, x = placed piece
        // o o o        x x x
        // o c c   ->   x c c
        // o c c        x c c

        let board: Vec<Vec<bool>> = vec![vec![1, 1, 1, 0], vec![0, 0, 1, 1]]
            .iter()
            .map(|y| y.iter().map(|x| *x == 1).collect())
            .collect();
        let expected_board = Board(vec![
            vec![
                0b_110_1_0_1111,
                0b_110_1_0_1111,
                0b_110_1_0_1111,
                0b_000_0_0_0000,
            ],
            vec![
                0b_000_0_0_0000,
                0b_000_0_0_0000,
                0b_110_1_0_1111,
                0b_110_1_0_1111,
            ],
        ]);

        // attempt puzzle solution
        let board = solve_puzzle(&board, &pieces, false).expect("Failed to solve puzzle");
        assert_eq!(expected_board.0, board.0);

        // test a non-solvable board:
        let board: Vec<Vec<bool>> = vec![vec![0, 1, 1], vec![1, 0, 0], vec![1, 0, 1]]
            .iter()
            .map(|y| y.iter().map(|x| *x == 1).collect())
            .collect();

        // attempt puzzle solution
        let board = solve_puzzle(&board, &pieces, false);
        assert_eq!(board.is_err(), true);
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
        let res = solve_puzzle(&board, &pieces, true).expect("Failed to solve puzzle");
        assert_eq!(expected_board.0, res.0);
    }
}
