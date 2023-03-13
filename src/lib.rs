pub struct Error;

// TODO make input generic
//
// TODO support non-uniform vector size piece inputs
//  ((1, 1, 0),
//   (1))
//
// TODO support more than 8 pieces input
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

// TODO implement recursion once tracer round complete
fn attempt_move(
    board: &mut Vec<Vec<u16>>,
    pieces_permuted: &mut Vec<Vec<Vec<Vec<u8>>>>,
    pieces_used: &mut Vec<u8>,
) -> Result<Vec<Vec<u16>>, Error> {
    Ok(vec![vec![0, 1], vec![1, 0]])
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Piecemeal {
        piece: Vec<Vec<bool>>,
        piece_flipped: Option<Vec<Vec<bool>>>,
        piece_rotated: Option<Vec<Vec<bool>>>,
        piece_encoded: Option<Vec<Vec<u8>>>,
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
        let mut pieces_permuted = calendar_pieces()
            .into_iter()
            .filter(|p| p.piece_permuted.is_some())
            .map(|p| p.piece_permuted.unwrap())
            .collect();

        // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
        let mut board: Vec<Vec<u16>> = vec![
            vec![0b_000_1_1_1001, 0b_000_1_1_1000, 0b_000_1_1_1110],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0011, 0b_000_1_1_0110, 0b_000_0_1_0000],
        ];
        let expected_board: Vec<Vec<u16>> = vec![
            vec![0b_000_1_1_1101, 0b_001_1_0_1000, 0b_001_1_0_1110],
            vec![0b_000_1_1_0101, 0b_001_1_0_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0101, 0b_001_1_0_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0111, 0b_001_1_0_0110, 0b_000_0_1_0000],
        ];

        // construct used pieces
        let mut pieces_used: Vec<u8> = vec![3];
        let expected_pieces_used: Vec<u8> = vec![3, 1];

        // attempt the move
        if let Ok(board) = attempt_move(&mut board, &mut pieces_permuted, &mut pieces_used) {
            assert_eq!(expected_board, board);
            assert_eq!(expected_pieces_used, pieces_used);
        } else {
            panic!("failed to attempt move")
        }
    }
}
