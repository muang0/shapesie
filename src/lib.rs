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

    #[test]
    fn flips_piece() {
        let l: Vec<Vec<bool>> = vec!(
            vec!(1, 0, 0),
            vec!(1, 0, 0),
            vec!(1, 1, 1)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let l_flipped: Vec<Vec<bool>> = vec!(
            vec!(1, 1, 1),
            vec!(1, 0, 0),
            vec!(1, 0, 0)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let staircase: Vec<Vec<bool>> = vec!(
            vec!(1, 0, 0, 0),
            vec!(0, 1, 0, 0),
            vec!(0, 0, 1, 1)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let staircase_flipped: Vec<Vec<bool>> = vec!(
            vec!(0, 0, 1, 1),
            vec!(0, 1, 0, 0),
            vec!(1, 0, 0, 0)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        
            assert_eq!(l_flipped, flip_piece(&l));
            assert_eq!(staircase_flipped, flip_piece(&staircase));
        }

    #[test]
    fn rotates_piece() {
        let field_goal: Vec<Vec<bool>> = vec!(
            vec!(1, 0, 1),
            vec!(1, 0, 1),
            vec!(0, 1, 0),
            vec!(0, 1, 0)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let field_goal_rotated: Vec<Vec<bool>> = vec!(
            vec!(0, 0, 1, 1),
            vec!(1, 1, 0, 0),
            vec!(0, 0, 1, 1)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let z: Vec<Vec<bool>> = vec!(
            vec!(1, 1, 1, 1),
            vec!(0, 0, 1, 0),
            vec!(0, 1, 0, 0),
            vec!(1, 1, 1, 1)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
        let z_rotated: Vec<Vec<bool>> = vec!(
            vec!(1, 0, 0, 1),
            vec!(1, 1, 0, 1),
            vec!(1, 0, 1, 1),
            vec!(1, 0, 0, 1)).iter().map(|x| x.iter().map(|y| *y == 1 ).collect()).collect();
    
        assert_eq!(field_goal_rotated, rotate_piece(&field_goal));
        assert_eq!(z_rotated, rotate_piece(&z));
    }
}
