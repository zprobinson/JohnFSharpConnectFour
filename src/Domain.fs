module Domain

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardRow = int
type BoardColumn = int
type Board = BoardSlot [,]
/// coordinates starting at 0,0 from the top left
type BoardPosition = BoardRow * BoardColumn
type GameOverStatus =
    | Win of Player
    | Tie
type BoardStatus = 
    | GameOver of GameOverStatus
    | StillGoing

type PlayerMoveGetter = Board -> BoardColumn
type GetPlayerMoveGetter = Player -> PlayerMoveGetter

let displayColsIndexedBy = 1
let firstColIndex = 0
let lastColIndex = 6
let firstDisplayCol = firstColIndex + displayColsIndexedBy
let lastDisplayCol = lastColIndex + displayColsIndexedBy
let numCols = lastColIndex + 1

let firstRowIndex = 0
let lastRowIndex = 5
let numRows = lastRowIndex + 1

let xChip = 'X'
let oChip = 'O'
let emptySlot = '.'

let emptyBoard:Board = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

let boardDisplayFormat =
    "\
|@ @ @ @ @ @ @|
|@ @ @ @ @ @ @|
|@ @ @ @ @ @ @|
|@ @ @ @ @ @ @|
|@ @ @ @ @ @ @|
|@ @ @ @ @ @ @|
---------------
 1 2 3 4 5 6 7 "
