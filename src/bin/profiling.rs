use shapesie;

fn main() {
    // define pieces
    #[rustfmt::skip]
    let pieces = vec!(
        vec!(
            vec!(1, 1),
            vec!(1, 0),
            vec!(1, 0),
            vec!(1, 0)
        ),
        vec!(
            vec!(1, 0),
            vec!(1, 1),
            vec!(1, 0),
            vec!(1, 0)
        ),
        vec!(
            vec!(1, 0, 1),
            vec!(1, 1, 1),
        ),
        vec!(
            vec!(0, 0, 1),
            vec!(1, 1, 1),
            vec!(1, 0, 0),
        ),
        vec!(
            vec!(1, 1, 1),
            vec!(1, 1, 1),
        ),
        vec!(
            vec!(1, 0, 0),
            vec!(1, 0, 0),
            vec!(1, 1, 1),
        ),
        vec!(
            vec!(0, 0, 1, 1),
            vec!(1, 1, 1, 0),
        ),
        vec!(
            vec!(1, 1, 1),
            vec!(0, 1, 1),
        ),
    ).iter().map(|piece| piece.iter().map(|x_arr| x_arr.iter().map(|x| *x == 1).collect()).collect()).collect();

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
            shapesie::solve_puzzle(&board, &pieces, true).expect("Unable to solve puzzle");
        }
    }
}
