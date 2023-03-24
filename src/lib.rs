pub struct Error;
enum Grid {
    Board,
    Piece,
} // TODO include struct in enum once implemented

// TODO make input generic
// TODO support non-uniform vector size piece inputs
//  ((1, 1, 0),
//   (1))
// TODO support more than 8 pieces input
// TODO implement recursion once tracer round complete
// TODO board/pieces as custom type & implement std::fmt::Binary

pub fn solve_puzzle(
    board: &Vec<Vec<bool>>,
    pieces: &Vec<Vec<Vec<bool>>>,
) -> Result<Vec<Vec<u16>>, Error> {
    Ok(vec![vec![0, 1], vec![1, 0]])
}

fn permute_pieces(pieces: &Vec<Vec<Vec<bool>>>) -> Result<Vec<Vec<Vec<Vec<u8>>>>, Error> {
    Ok(vec![vec![vec![vec![0, 1], vec![1, 0]]]])
}

fn encode_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<u8>> {
    vec![vec![0b0]]
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

fn calculate_neighbors(grid: &mut Vec<Vec<u16>>, grid_type: Grid) {
    let grid_reference = grid.clone(); // avoid performance penalty by using unsafe rust when updating grid?
    match grid_type {
        Grid::Board => {
            for (y_index, x_arr) in grid.iter_mut().enumerate() {
                for (x_index, square) in x_arr.iter_mut().enumerate() {
                    // don't need to calculate neighbors for off-board squares
                    if *square | 0b_111_0_1_1111 == 0b_111_1_1_1111 {
                        // avoid subtraction overflow if y_index is north edge
                        if y_index == 0 {
                            *square = *square | 0b_000_0_0_1000;
                        } else {
                            // get north square
                            if let Some(x_arr2) = grid_reference.get(y_index - 1) {
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
                        if let Some(x_arr2) = grid_reference.get(y_index) {
                            if let Some(north_square) = x_arr2.get(x_index + 1) {
                                // if the east square is off the board or blocked
                                if *north_square & 0b_1_1_0000 != 0b_1_1_0000 {
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
                        if let Some(x_arr2) = grid_reference.get(y_index + 1) {
                            if let Some(north_square) = x_arr2.get(x_index) {
                                // if the south square is off the board or blocked
                                if *north_square & 0b_1_1_0000 != 0b_1_1_0000 {
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
                            if let Some(x_arr2) = grid_reference.get(y_index) {
                                if let Some(north_square) = x_arr2.get(x_index - 1) {
                                    // if the west square is off the board or blocked
                                    if *north_square & 0b_1_1_0000 != 0b_1_1_0000 {
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
        Grid::Piece => {}
    }
}

fn attempt_move(
    board: Vec<Vec<u16>>,
    pieces_permuted: Vec<Vec<Vec<Vec<u8>>>>,
) -> Result<Vec<Vec<u16>>, Error> {
    // find first 'most restricted' space
    // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
    // 1st 3-walled space
    let mut target_space = board.iter().enumerate().find_map(|(y_index, x_vec)| {
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
        target_space = board.iter().enumerate().find_map(|(y_index, x_vec)| {
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
        for piece_permutations in pieces_permuted.iter() {
            for piece in piece_permutations.iter() {
                for (y_index_piece_anchor, x_arr) in piece.iter().enumerate() {
                    for (x_index_piece_anchor, piece_space_anchor) in x_arr.iter().enumerate() {
                        // piece anchor should fit onto the board's target space without any neighbor/wall conflicts
                        if piece_space_anchor & 0b_1_0000 == 0b_1_0000
                            && !u16::from(*piece_space_anchor)
                                & (board[y_index_board][x_index_board] & 0b_1111)
                                == (board[y_index_board][x_index_board] & 0b_1111)
                        {
                            // determine if piece is a valid move (given fixed board target space & piece anchor)
                            let piece_is_blocked = piece.iter().enumerate().any(|(y_index_piece, x_arr2)| x_arr2.iter().enumerate().any(|(x_index_piece, piece_space)|
                                // check if piece vector item corresponds to a piece square
                                if piece_space & 0b_1_0000 == 0b_1_0000 {
                                    // check if associated board square is open
                                    if let Some(x_arr2) = board.get(y_index_board - y_index_piece_anchor + y_index_piece) {
                                        if let Some(piece) = x_arr2.get(x_index_board - x_index_piece_anchor + x_index_piece) {
                                            if piece & 0b_1_1_0000 != 0b_1_1_0000 {
                                                return true
                                            }
                                        }
                                    } return false
                                } else { return false }
                            ));
                            if !piece_is_blocked {
                                let mut updated_board = board.clone();
                                // update board to reflect piece placement
                                for (y_index_piece, x_arr2) in piece.iter().enumerate() {
                                    for (x_index_piece, piece_space) in x_arr2.iter().enumerate() {
                                        if piece_space & 0b_1_0000 == 0b_1_0000 {
                                            if let Some(x_arr3) = updated_board.get_mut(
                                                y_index_board - y_index_piece_anchor
                                                    + y_index_piece,
                                            ) {
                                                if let Some(piece) = x_arr3.get_mut(
                                                    x_index_board - x_index_piece_anchor
                                                        + x_index_piece,
                                                ) {
                                                    *piece = 0b_001_1_0_0000 // TODO how to set piece UID? Bubble up to library api?
                                                } else {
                                                    return Err(Error);
                                                }
                                            } else {
                                                return Err(Error);
                                            }
                                        }
                                    }
                                }
                                calculate_neighbors(&mut updated_board, Grid::Board);
                                return Ok(updated_board);
                            }
                        }
                    }
                }
            }
        }
    } else {
        return Err(Error); // TODO
    }
    return Err(Error); // TODO
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Piecemeal {
        piece: Vec<Vec<bool>>,
        piece_flipped: Option<Vec<Vec<bool>>>,
        piece_rotated: Option<Vec<Vec<bool>>>,
        piece_encoded: Option<Vec<Vec<u8>>>, // TODO, can instead use piece_permuted[0] for testing encoding func?
        piece_permuted: Option<Vec<Vec<Vec<u8>>>>,
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
                    vec!(0b_1_0110, 0b_1_0001),
                    vec!(0b_1_1010, 0b_0_1001),
                    vec!(0b_1_1010, 0b_0_0001),
                    vec!(0b_1_1000, 0b_0_0001),
                )),
                piece_permuted: Some(vec!(
                    vec!(
                        vec!(0b_1_0100, 0b_1_0101, 0b_1_0101, 0b_1_0011),
                        vec!(0b_0_1000, 0b_0_1000, 0b_0_1100, 0b_1_1000),
                    ),
                    vec!(
                        vec!(0b_1_0110, 0b_1_0001),
                        vec!(0b_1_1010, 0b_0_1001),
                        vec!(0b_1_1010, 0b_0_0001),
                        vec!(0b_1_1000, 0b_0_0001),
                    ),                    
                )),  // TODO implement piece_permuted correctly & fully after tracer round
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
                piece_permuted: None
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
    fn places_piece() {
        // we want to test the following placement, where:
        //  o = open space, c = closed space, x = placed piece
        // o o o        o x x
        // o o c   ->   o x c
        // o o c        o x c
        // o c c        o x c

        // collect permuted pieces
        let pieces_permuted = calendar_pieces()
            .into_iter()
            .filter(|p| p.piece_permuted.is_some())
            .map(|p| p.piece_permuted.unwrap())
            .collect();

        // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
        let board: Vec<Vec<u16>> = vec![
            vec![0b_000_1_1_1001, 0b_000_1_1_1000, 0b_000_1_1_1110],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0011, 0b_000_1_1_0110, 0b_000_0_1_0000],
        ];
        let expected_board: Vec<Vec<u16>> = vec![
            vec![0b_000_1_1_1101, 0b_001_1_0_1110, 0b_001_1_0_1111],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_0000],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_0000],
            vec![0b_000_1_1_0111, 0b_001_1_0_1110, 0b_000_0_1_0000],
        ];

        // attempt piece placement
        if let Ok(board) = attempt_move(board, pieces_permuted) {
            assert_eq!(expected_board, board);
        } else {
            panic!("failed to attempt move")
        }
    }
}
