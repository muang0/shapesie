use std::fmt;

/// A type that represents errors that could be encountered when solving a placement puzzle
#[derive(Debug)]
pub enum Error {
    NoSolutionFound,
    NoTargetSpaceFound,
    BoardTooTall,
    BoardTooWide,
    PieceTooTall,
    PieceTooWide,
    TooManyPieces,
}

/// A type that represents a game board where puzzle pieces can be placed
#[derive(Clone, Debug, PartialEq)]
pub struct Board(
    /// Serialization: {8b': uid, 1b': is_board, 1'b: is_open, 4b': neighbors}
    pub Vec<Vec<u16>>,
);

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
    /// Validates that the board meets solvability constraints
    pub fn validate_board(&self) -> Result<(), Error> {
        // upper bound on width/height is half of usize::MAX due to design decision of converting usize->isize when calculating neighbors (this allows us to stay DRY and loop through offset tuple array).  Potentially revisit this in the future.
        let board_width_bound = (usize::MAX / 2) - 1;
        if self.0.len() >= board_width_bound {
            return Err(Error::BoardTooTall);
        }
        for x_arr in self.0.iter() {
            if x_arr.len() >= board_width_bound {
                return Err(Error::BoardTooWide);
            }
        }
        Ok(())
    }

    // support both 'validate' & 'validate_board' to avoid a wasteful major release.
    pub fn validate(&self) -> Result<(), Error> {
        // upper bound on width/height is half of usize::MAX due to design decision of converting usize->isize when calculating neighbors (this allows us to stay DRY and loop through offset tuple array).  Potentially revisit this in the future.
        let board_width_bound = (usize::MAX / 2) - 1;
        if self.0.len() >= board_width_bound {
            return Err(Error::BoardTooTall);
        }
        for x_arr in self.0.iter() {
            if x_arr.len() >= board_width_bound {
                return Err(Error::BoardTooWide);
            }
        }
        Ok(())
    }

    // calculates 'neighbors' serialized field for each square on the board
    // each bit in this field represents if a neighbor exists on a particular side of the space
    // a neighbor could be a placed piece or the edge of the board  ☝️👉👇👈
    fn initialize_neighbors(&mut self) {
        let board_reference = self.0.clone();
        for (y_index, x_arr) in self.0.iter_mut().enumerate() {
            for (x_index, square) in x_arr.iter_mut().enumerate() {
                // don't need to calculate neighbors for off-board squares
                if *square | 0b_11111111_0_1_1111 == 0b_11111111_1_1_1111 {
                    let y_index_signed = y_index as isize; // already validated max board input width above
                    let x_index_signed = x_index as isize;
                    let offsets: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
                    for (offset_index, (y_offset, x_offset)) in offsets.iter().enumerate() {
                        let neighbor_open_encoding =
                            (0b_111_0111 >> offset_index) | 0b_1111_1111_1111_0000;
                        let neighbor_closed_encoding = !neighbor_open_encoding;
                        if let Some(x_arr2) =
                            board_reference.get((y_index_signed + *y_offset) as usize)
                        {
                            if let Some(neighbor) =
                                x_arr2.get((x_index_signed + *x_offset) as usize)
                            {
                                if neighbor & 0b_1_1_0000 != 0b_1_1_0000 {
                                    *square = *square | neighbor_closed_encoding;
                                // neighbor is off the board or blocked
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

    fn update_local_neighbors(&mut self, y_index: usize, x_index: usize) {
        if let Some(x_arr) = self.0.get_mut(y_index) {
            if let Some(square) = x_arr.get_mut(x_index) {
                // if square is not a valid, open space
                if *square & 0b_1_1_0000 != 0b_1_1_0000 {
                    // update south square
                    if let Some(y_target_index) = y_index.checked_add(1) {
                        if let Some(target_x_arr) = self.0.get_mut(y_target_index) {
                            if let Some(target_piece) = target_x_arr.get_mut(x_index) {
                                *target_piece = *target_piece | 0b_1000;
                            }
                        }
                    }
                    // update west square
                    if let Some(x_target_index) = x_index.checked_sub(1) {
                        if let Some(target_x_arr) = self.0.get_mut(y_index) {
                            if let Some(target_piece) = target_x_arr.get_mut(x_target_index) {
                                *target_piece = *target_piece | 0b_0100;
                            }
                        }
                    }
                    // update north square
                    if let Some(y_target_index) = y_index.checked_sub(1) {
                        if let Some(target_x_arr) = self.0.get_mut(y_target_index) {
                            if let Some(target_piece) = target_x_arr.get_mut(x_index) {
                                *target_piece = *target_piece | 0b_0010;
                            }
                        }
                    }
                    // update east square
                    if let Some(x_target_index) = x_index.checked_add(1) {
                        if let Some(target_x_arr) = self.0.get_mut(y_index) {
                            if let Some(target_piece) = target_x_arr.get_mut(x_target_index) {
                                *target_piece = *target_piece | 0b_0001;
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A type that represents a particular orientation of a piece
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Placement(
    /// Serialization : {1b': is_piece, 4b': neighbors}
    pub Vec<Vec<u8>>,
);

impl Placement {
    /// Validates the placement meets usability constraints
    pub fn validate(&self) -> Result<(), Error> {
        // upper bound on width/height is half of usize::MAX due to design decision of converting usize->isize when calculating neighbors (this allows us to loop through offset tuple array).  Potentially revisit this in the future.
        let piece_width_bound = (usize::MAX / 2) - 1;
        if self.0.len() >= piece_width_bound {
            return Err(Error::PieceTooTall);
        }
        for x_arr in self.0.iter() {
            if x_arr.len() >= piece_width_bound {
                return Err(Error::PieceTooWide);
            }
        }
        Ok(())
    }

    // copies and flips a placement
    fn flip(&self) -> Result<Placement, Error> {
        let piece: Vec<Vec<bool>> = self
            .0
            .iter()
            .rev()
            .map(|x_arr| {
                x_arr
                    .iter()
                    .map(|square| *square & 0b_1_0000 == 0b_1_0000)
                    .collect()
            })
            .collect();
        piece.to_placement()
    }

    // copies and rotates a placement (90 degrees clockwise)
    fn rotate(&self) -> Result<Placement, Error> {
        let mut rotated_placement = vec![vec![false; self.0.len()]; self.0[0].len()];
        for (y_index, x_vec) in self.0.iter().rev().enumerate() {
            for (x_index, val) in x_vec.iter().enumerate() {
                if val & 0b_1_0000 == 0b_1_0000 {
                    rotated_placement[x_index][y_index] = true;
                }
            }
        }
        rotated_placement.to_placement()
    }

    // calculates 'neighbors' serialized field for each square on the piece vector
    // if the square represents a piece => Each bit in this field represents if an **open space** exists on a particular side of the space ☝️👉👇👈
    fn initialize_neighbors(&mut self) {
        let piece_reference = self.0.clone();
        for (y_index, x_arr) in self.0.iter_mut().enumerate() {
            for (x_index, square) in x_arr.iter_mut().enumerate() {
                // don't need to calculate neighbors for off-board squares
                if *square & 0b_1_0000 == 0b_1_0000 {
                    let y_index_signed = y_index as isize; // already validated max piece input width above
                    let x_index_signed = x_index as isize;
                    let offsets: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
                    for (offset_index, (y_offset, x_offset)) in offsets.iter().enumerate() {
                        let neighbor_open_encoding = (0b_1111_0111 >> offset_index) | 0b_1111_0000;
                        let neighbor_closed_encoding = !neighbor_open_encoding;
                        if let Some(x_arr2) =
                            piece_reference.get((y_index_signed + *y_offset) as usize)
                        {
                            if let Some(neighbor) =
                                x_arr2.get((x_index_signed + *x_offset) as usize)
                            {
                                if neighbor & 0b_1_0000 != 0b_1_0000 {
                                    *square = *square | neighbor_closed_encoding;
                                // neighbor is not a piece
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
}

impl fmt::Binary for Placement {
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

/// A trait for converting a value into a `Placement`
pub trait ToPlacement {
    fn to_placement(self) -> Result<Placement, Error>;
}

impl ToPlacement for &Vec<Vec<bool>> {
    fn to_placement(self) -> Result<Placement, Error> {
        let mut encoded_placement = Placement(
            self.iter()
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
        encoded_placement.initialize_neighbors();
        encoded_placement.validate()?;
        Ok(encoded_placement)
    }
}

/// A type that represents all `Placements` of a given piece
#[derive(Clone, Debug, PartialEq)]
pub struct Piece {
    pub uid: u8,                    // used for identifying the piece once placed on the board
    pub placements: Vec<Placement>, // all possible unique orientations of the piece
}

/// A type that represents all `Pieces` to be placed
#[derive(Clone)]
pub struct Hand {
    pub pieces: Vec<Piece>,
}

/// A trait for converting a value into a `Board`
pub trait ToBoard {
    fn to_board(self) -> Result<Board, Error>;
}

impl ToBoard for &Vec<Vec<bool>> {
    fn to_board(self) -> Result<Board, Error> {
        let mut board = Board(
            self.iter()
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
        board.initialize_neighbors();
        board.validate()?;
        Ok(board)
    }
}

impl Hand {
    pub fn validate(&self) -> Result<(), Error> {
        if self.pieces.len() > (2 ^ 8) {
            return Err(Error::TooManyPieces);
        }
        Ok(())
    }

    fn remove_piece_by_uid(&mut self, uid: u8) {
        if let Some(piece_index) = self.pieces.iter().position(|piece| piece.uid == uid) {
            self.pieces.remove(piece_index);
        }
    }
}

/// A trait for converting a value into a `Hand`
pub trait ToHand {
    fn to_hand(self) -> Result<Hand, Error>;
}

impl ToHand for &Vec<Vec<Vec<bool>>> {
    fn to_hand(self) -> Result<Hand, Error> {
        let mut hand = Hand { pieces: vec![] };
        // loop through pieces
        for (index, raw_piece) in self.clone().iter_mut().enumerate() {
            // zero extend rows to uniform length
            match raw_piece.iter().map(|x_arr| x_arr.len()).max() {
                Some(row_length) => raw_piece
                    .iter_mut()
                    .for_each(|x_arr| x_arr.resize(row_length, false)),
                None => continue,
            }
            // rotate & encode piece 4x
            let uid_result = u8::try_from(index);
            if uid_result.is_err() {
                return Err(Error::TooManyPieces);
            }
            let mut encoded_piece = Piece {
                uid: uid_result.unwrap(),
                placements: Vec::new(),
            };
            encoded_piece.placements.push(raw_piece.to_placement()?);
            let mut rotated_placement = raw_piece.to_placement()?;
            for _ in 0..3 {
                rotated_placement = rotated_placement.rotate()?;
                encoded_piece.placements.push(rotated_placement.clone())
            }
            // flip piece, then rotate & encode 4x
            encoded_piece
                .placements
                .push(raw_piece.to_placement()?.flip()?);
            let mut flipped_rotated_placement = raw_piece.to_placement()?.flip()?;
            for _ in 0..3 {
                flipped_rotated_placement = flipped_rotated_placement.rotate()?;
                encoded_piece
                    .placements
                    .push(flipped_rotated_placement.clone())
            }
            // remove duplicate piece permutations
            encoded_piece.placements.sort();
            encoded_piece.placements.dedup();
            hand.pieces.push(encoded_piece)
        }
        hand.validate()?;
        Ok(hand)
    }
}

/// solve the puzzle by attempting to place all `Pieces` onto the board in a non-overlapping manner.
pub fn solve_puzzle<T: ToBoard, Y: ToHand>(board: T, hand: Y) -> Result<Board, Error> {
    let hand = <Y as ToHand>::to_hand(hand)?;
    let board: Board = <T as ToBoard>::to_board(board)?;
    // recursively attempt moves until solution is found or search space has been exhausted
    return attempt_move(&board, &hand, true);
}

// for a given board, generate a list of the most restricted spaces
fn find_target_spaces(board: &Board) -> Result<Vec<(usize, usize)>, Error> {
    // find target spaces for attempting piece placement
    // order of search is: 4-walled spaces (islands), 3-walled spaces (nooks), 2-walled spaces (corners)
    let target_space_criteria: Vec<Vec<u16>> = vec![
        vec![0b1111],                         // island
        vec![0b0111, 0b1011, 0b1101, 0b1110], // nooks
        vec![0b0011, 0b1001, 0b1100, 0b0110], // corners
        vec![0b0001, 0b0010, 0b0100, 0b1000], // edges
    ];
    let mut target_spaces: Vec<(usize, usize)> = vec![];
    for criteria in target_space_criteria.iter() {
        if let Some(target_space_indexes) =
            board.0.iter().enumerate().find_map(|(y_index, x_vec)| {
                x_vec.iter().enumerate().find_map(|(x_index, space)| {
                    for neighbor_encoding in criteria.iter() {
                        // target space will be a valid, open square and match the neighbor_encoding criteria
                        if space & (neighbor_encoding | 0b_1_1_0000)
                            == (neighbor_encoding | 0b_1_1_0000)
                        {
                            return Some((y_index, x_index));
                        }
                    }
                    None
                })
            })
        {
            if !target_spaces.contains(&target_space_indexes) {
                let mut target_space_index: Vec<(usize, usize)> = vec![target_space_indexes];
                target_spaces.append(&mut target_space_index);
            }
        }
    }
    if target_spaces.len() == 0 {
        return Err(Error::NoTargetSpaceFound);
    }
    Ok(target_spaces)
}

fn attempt_move(board: &Board, hand: &Hand, should_recurse: bool) -> Result<Board, Error> {
    // find target_spaces
    for (y_index_board, x_index_board) in find_target_spaces(board).unwrap() { // TODO: solve more elegantly than unwrap
        // find a piece that has a valid anchor point (⚓️ the piece onto the board by finding a square on the piece that fits the target space without any neighbor conflicts)
        let mut updated_hand: Hand;
        let mut updated_board: Board;
        for piece in hand.pieces.iter() {
            // for each possible orientation of a given piece
            for placement in piece.placements.iter() {
                // 👀 through the placement squares for an ⚓️ point
                for (y_index_piece_anchor, x_arr) in placement.0.iter().enumerate() {
                    for (x_index_piece_anchor, piece_space_anchor) in x_arr.iter().enumerate() {
                        // placement ⚓️ should fit onto the board's target space without any neighbor/wall conflicts
                        if piece_space_anchor & 0b_1_0000 == 0b_1_0000
                            && u16::from(*piece_space_anchor)
                                & (board.0[y_index_board][x_index_board] & 0b_1111)
                                == (board.0[y_index_board][x_index_board] & 0b_1111)
                        {
                            // determine if placement is a valid move (given fixed board target space & placement ⚓️)
                            let piece_is_blocked = placement.0.iter().enumerate().any(|(y_index_piece, x_arr2)| x_arr2.iter().enumerate().any(|(x_index_piece, piece_space)|
                                // check if placement vector item corresponds to a placement square
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
                                } else { return false }  // placement vector item is an empty space, which is always a valid overlay
                            ));
                            // if the placement fits, update the board accordingly
                            if !piece_is_blocked {
                                updated_board = Board(board.0.clone());
                                for (y_index_piece, x_arr2) in placement.0.iter().enumerate() {
                                    for (x_index_piece, piece_space) in x_arr2.iter().enumerate() {
                                        if piece_space & 0b_1_0000 == 0b_1_0000 {
                                            // don't have to worry about subraction overflow here since already checked above
                                            let target_y_index = y_index_board + y_index_piece - y_index_piece_anchor;
                                            if let Some(x_arr3) =
                                                updated_board.0.get_mut(target_y_index)
                                            {
                                                let target_x_index = x_index_board + x_index_piece
                                                    - x_index_piece_anchor;
                                                if let Some(board_square) =
                                                    x_arr3.get_mut(target_x_index)
                                                {
                                                    // serialize board square as taken by given piece uid
                                                    *board_square = (*board_square & 0b_1_0_1111)
                                                        | u16::from(piece.uid) << 6;
                                                    updated_board.update_local_neighbors(
                                                        target_y_index,
                                                        target_x_index,
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                                updated_hand = hand.clone();
                                updated_hand.remove_piece_by_uid(piece.uid);
                                // return updated board if all pieces have been used (or if recursion is disabled)
                                if updated_hand.pieces.len() == 0 || !should_recurse {
                                    return Ok(updated_board);
                                }
                                // recursively attempt moves
                                let move_res = attempt_move(&updated_board, &updated_hand, should_recurse);
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
    }
    return Err(Error::NoSolutionFound);
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Piecemeal {
        raw_piece: Vec<Vec<bool>>,
        encoded_placement_flipped: Option<Placement>,
        encoded_placement_rotated: Option<Placement>,
        encoded_placement: Option<Placement>,
        encoded_piece: Option<Piece>,
    }

    #[rustfmt::skip]
    fn calendar_pieces() -> Vec<Piecemeal> {
        vec!(
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 1),
                    vec!(1),
                    vec!(1),
                    vec!(1)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: Some(Placement(vec!(
                    vec!(0b_1_1101),
                    vec!(0b_1_0101),
                    vec!(0b_1_0101),
                    vec!(0b_1_0011, 0b_1_1110),
                ))),
                encoded_placement_rotated: Some(Placement(vec!(
                    vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_1100),
                    vec!(0b_0_0000, 0b_0_0000, 0b_0_0000, 0b_1_0111)
                ))),
                encoded_placement: Some(Placement(vec!(
                    vec!(0b_1_1001, 0b_1_1110),
                    vec!(0b_1_0101),
                    vec!(0b_1_0101),
                    vec!(0b_1_0111),
                ))),
                encoded_piece: Some(
                    Piece {
                        uid: 1,
                        placements: vec!(
                            Placement(vec!(
                                vec!(0b_0_0000, 0b_0_0000, 0b_0_0000, 0b_1_1101),
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_0110),
                            )),
                            Placement(vec!(
                                vec!(0b_0_0000, 0b_1_1101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_1_1011, 0b_1_0110),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1001, 0b_1_1010, 0b_1_1010, 0b_1_1110),
                                vec!(0b_1_0111, 0b_0_0000, 0b_0_0000, 0b_0_0000),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0111, 0b_0_0000),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1011, 0b_1_1010, 0b_1_1010, 0b_1_1100),
                                vec!(0b_0_0000, 0b_0_0000, 0b_0_0000, 0b_1_0111),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1011, 0b_1_1100),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_0_0000, 0b_1_0111),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1110),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1101, 0b_0_0000, 0b_0_0000, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1010, 0b_1_1010, 0b_1_1110),
                            )),
                        ),
                    },
                ),
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 0),
                    vec!(1, 1),
                    vec!(1),
                    vec!(1, 0)
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 0, 1),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: Some(Placement(vec!(
                    vec!(0b_1_1001, 0b_1_1010, 0b_1_1100),
                    vec!(0b_1_0111, 0b_0_0000, 0b_1_0111)
                ))),
                encoded_placement_rotated: Some(Placement(vec!(
                    vec!(0b_1_1001, 0b_1_1110),
                    vec!(0b_1_0101, 0b_0_0000),
                    vec!(0b_1_0011, 0b_1_1110),
                ))),
                encoded_placement: None,
                encoded_piece: Some(
                    Piece { 
                        uid: (2),
                        placements: (vec!(
                            Placement(vec!(
                                vec!(0b_1_1001, 0b_1_1010, 0b_1_1100),
                                vec!(0b_1_0111, 0b_0_0000, 0b_1_0111),
                            )),
                            Placement(vec!(
                                vec!(0b_1_1001, 0b_1_1110),
                                vec!(0b_1_0101, 0b_0_0000),
                                vec!(0b_1_0011, 0b_1_1110)
                            )),
                            Placement(vec!(
                                vec!(0b_1_1011, 0b_1_1100),
                                vec!(0b_0_0000, 0b_1_0101),
                                vec!(0b_1_1011, 0b_1_0110)
                            )),
                            Placement(vec!(
                                vec!(0b_1_1101, 0b_0_0000, 0b_1_1101),
                                vec!(0b_1_0011, 0b_1_1010, 0b_1_0110),
                            )),
                        ))
                    }
                )
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(0, 0, 1),
                    vec!(1, 1, 1),
                    vec!(1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 1, 1),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 0, 0),
                    vec!(1),
                    vec!(1, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(0, 0, 1, 1),
                    vec!(1, 1, 1, 0),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            },
            Piecemeal {
                raw_piece: vec!(
                    vec!(1, 1, 1),
                    vec!(0, 1, 1),
                ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect(),
                encoded_placement_flipped: None,
                encoded_placement_rotated: None,
                encoded_placement: None,
                encoded_piece: None
            }
        )
    }

    #[test]
    fn flips_placement() {
        for piecemeal in calendar_pieces() {
            if let Some(encoded_placement_flipped) = piecemeal.encoded_placement_flipped {
                assert_eq!(
                    encoded_placement_flipped,
                    piecemeal.raw_piece.to_placement().unwrap().flip().unwrap()
                );
            }
        }
    }

    #[test]
    fn rotates_placement() {
        for piecemeal in calendar_pieces() {
            if let Some(encoded_placement_rotated) = piecemeal.encoded_placement_rotated {
                assert_eq!(
                    encoded_placement_rotated,
                    piecemeal
                        .raw_piece
                        .to_placement()
                        .unwrap()
                        .rotate()
                        .unwrap()
                );
            }
        }
    }

    #[test]
    fn encodes_piece() {
        for piecemeal in calendar_pieces() {
            if let Some(encoded_placement) = piecemeal.encoded_placement {
                assert_eq!(
                    encoded_placement,
                    piecemeal.raw_piece.to_placement().unwrap()
                );
            }
        }
    }

    #[test]
    fn collects_hand() {
        // collect hand from calendar_pieces where placements is defined
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .filter(|piecemeal| piecemeal.encoded_piece.is_some())
            .map(|piecemeal| piecemeal.raw_piece.clone())
            .collect();

        // permute pieces
        let hand = &pieces
            .to_hand()
            .expect("Should be able to generate hand from calendar puzzle pieces");

        // collect pieces_permuted from calendar_pieces
        let mut expected_pieces_permuted: Vec<Piece> = calendar_pieces()
            .iter()
            .filter(|piecemeal| piecemeal.encoded_piece.is_some())
            .map(|piecemeal| piecemeal.encoded_piece.clone().unwrap())
            .collect();

        // populate uid for expected permuted pieces
        for (index, piece) in expected_pieces_permuted.iter_mut().enumerate() {
            piece.uid = u8::try_from(index).expect("More than 256 pieces");
        }

        // validate
        for (index, piece) in hand.pieces.iter().enumerate() {
            assert_eq!(
                *expected_pieces_permuted
                    .get(index)
                    .expect("Number of expected pieces should equal the number of permuted pieces"),
                *piece
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
        // o o c        o x c

        // collect pieces into hand
        let hand = Hand {
            pieces: calendar_pieces()
                .into_iter()
                .filter(|p| p.encoded_piece.is_some())
                .map(|p| p.encoded_piece.unwrap())
                .collect(),
        };

        // board encoding scheme: {3b': piece_used, 1b': is_board, 1'b: is_open, 4b': neighbors}
        let board = Board(vec![
            vec![0b_000_1_1_1001, 0b_000_1_1_1000, 0b_000_1_1_1110],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0001, 0b_000_1_1_0100, 0b_000_0_1_0000],
            vec![0b_000_1_1_0011, 0b_000_1_1_0110, 0b_000_0_1_0000],
        ]);
        let expected_board = Board(vec![
            vec![0b_000_1_1_1101, 0b_001_1_0_1110, 0b_001_1_0_1111],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_1001],
            vec![0b_000_1_1_0101, 0b_001_1_0_1110, 0b_000_0_1_0001],
            vec![0b_000_1_1_0111, 0b_001_1_0_1110, 0b_000_0_1_0001],
        ]);

        // attempt piece placement
        let board: Board = attempt_move(&board, &hand, false).expect("Failed to attempt move");
        assert_eq!(expected_board, board);
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
            vec!(1, 1, 1)
        ).iter().map(|y| y.iter().map(|x| *x == 1).collect()).collect();
        
        let expected_board = Board(vec!(
            vec!(0b_000_1_0_1111, 0b_000_1_0_1111, 0b_000_0_0_0111, 0b_001_1_0_1111, 0b_011_1_0_1111, 0b_011_1_0_1111, 0b_000_0_0_0001),
            vec!(0b_000_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_001_1_0_1111, 0b_011_1_0_1111, 0b_000_0_0_0011),
            vec!(0b_000_1_0_1111, 0b_010_1_0_1111, 0b_010_1_0_1111, 0b_010_1_0_1111, 0b_110_1_0_1111, 0b_011_1_0_1111, 0b_011_1_0_1111),
            vec!(0b_000_1_0_1111, 0b_010_1_0_1111, 0b_111_1_0_1111, 0b_010_1_0_1111, 0b_110_1_0_1111, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_111_1_0_1111, 0b_111_1_0_1111, 0b_110_1_0_1111, 0b_110_1_0_1111, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_111_1_0_1111, 0b_111_1_0_1111, 0b_110_1_0_1111, 0b_000_0_0_1101, 0b_100_1_0_1111, 0b_100_1_0_1111),
            vec!(0b_101_1_0_1111, 0b_101_1_0_1111, 0b_101_1_0_1111),
        ));

        // collect calendar pieces
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .map(|piecemeal| piecemeal.raw_piece.clone())
            .collect();

        // solve puzzle
        let res = solve_puzzle(&board, &pieces).expect("Failed to solve puzzle");
        // print!("{}", res);
        assert_eq!(expected_board.0, res.0);
    }

    #[test]
    fn solves_all_possible_days() {
        // collect calendar pieces
        let pieces: Vec<Vec<Vec<bool>>> = calendar_pieces()
            .iter()
            .map(|piecemeal| piecemeal.raw_piece.clone())
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

    #[test]
    fn solves_imperfect_puzzle() {
        // custom piece for testing
        // against open-square solutions exist
        let pieces: Vec<Vec<Vec<bool>>> = vec![
            vec![
                vec![true, true, true],
                vec![true, true, true],
                vec![true, true, true]
            ],
            vec![
                vec![true, true, true],
                vec![true, true, true],
                vec![false, false, false]
            ]
        ];

        // iterate through all possible month, day combinations
        // define board
        #[rustfmt::skip]
        let board: Vec<Vec<bool>> = vec![
            vec![1, 1, 1, 1, 1, 1],
            vec![1, 1, 1, 1, 1, 1],
            vec![1, 1, 1, 1, 1, 1]
        ].iter()
        .map(|y| y.iter().map(|x| *x == 1).collect())
        .collect();

        solve_puzzle(&board, &pieces).expect("Unable to solve puzzle");
    }
}
