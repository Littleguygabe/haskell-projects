

data Piece = Zero | One

instance Show Piece where
  show Zero = "0"
  show One = "1"

type Row = [Piece]
type Board = [Row]

temp :: Board
temp = [[Zero,One,Zero],[One,One,One],[Zero,Zero,One]]





