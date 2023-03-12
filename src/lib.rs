pub struct Error;

// TODO make input generic
//
// TODO support non-uniform vector size piece inputs
//  ((1, 1, 0),
//   (1))
pub fn solve_puzzle(board: &Vec<Vec<bool>>, pieces: &Vec<Vec<Vec<bool>>>) -> Result<Vec<Vec<u16>>, Error> {
    Ok(vec!(vec!(0, 1),
                  vec!(1, 0)))
}

fn permute_pieces(pieces: &Vec<Vec<Vec<bool>>>) -> Result<Vec<Vec<Vec<Vec<u8>>>>, Error> {
    Ok(vec!(vec!(vec!(vec!(0, 1), vec!(1, 0)))))
}

fn encode_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<u8>> {
    vec!(vec!(0b0))
}

// copies and rotates a piece (90 degrees clockwise)
fn rotate_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let mut rotated_piece = vec![vec![false; piece.len()]; piece[0].len()];
    for (y_index, x_vec) in piece.iter().rev().enumerate() {
        for (x_index, val) in x_vec.iter().enumerate() {
            rotated_piece[x_index][y_index] = val.clone();
        }
    }
    return rotated_piece
}

// copies and flips a piece
fn flip_piece(piece: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    return piece.iter().rev().map(|y| y.clone()).collect();
}

fn attempt_move(board: &mut Vec<Vec<u16>>, pieces: &mut Vec<Vec<Vec<Vec<u8>>>>, pieces_used: &mut Vec<u8>) -> Result<Vec<Vec<u16>>, Error> {
    Ok(vec!(vec!(0, 1),
                     vec!(1, 0)))
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Piecemeal {
        piece: Vec<Vec<bool>>,
        piece_flipped: Option<Vec<Vec<bool>>>,
        piece_rotated: Option<Vec<Vec<bool>>>,
        piece_encoded: Option<Vec<Vec<u8>>>,
        piece_permuted: Option<Vec<Vec<Vec<u8>>>>
    }

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
                    vec!(0b10110, 0b10001),
                    vec!(0b11010, 0b01001),
                    vec!(0b11010, 0b00001),
                    vec!(0b11000, 0b00001),
                )),
                piece_permuted: None
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

}
