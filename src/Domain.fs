module Domain
open Utils

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardRow = int
type BoardColumn = int

// You could create what are called Simple types instead of structs above
// These are single case Discriminated Unions
type BoardRow' = BoardRow of int
type BoardColumn' = BoardColumn of int

// See what happens here:
let printBoardRowONLY (row : BoardRow) = printfn "%A" row
let redHerringBoardRow : BoardColumn = 12
// The communists win, their board column spies 
// have infiltrated our pure functions!!!
printBoardRowONLY redHerringBoardRow |> ignore

// Unless...?
let printBoardRowONLYForRealThisTime (row : BoardRow') = printfn "%A" row
// notice that we alias the type as BoardColumn in our DU
// it is a "BoardColumn" of int. The overall type itself is BoardColumn'
let redHerringBoardRow' : BoardColumn' = BoardColumn 12
// The allies win because we've leveraged Single Case Discriminated Unions!!
printBoardRowONLYForRealThisTime redHerringBoardRow'

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

// These above types are being named as if they are OBJECTS.
// They are currently NOUNs.
// But they are functions, which means they do sh*t.
// We should use VERBs!
type FindValidMoves = Board -> BoardColumn
type FindPlayersValidMoves = Player -> FindValidMoves

// Just like in C#, you can leverage Literals to increase compilation speed.
// Literals are treated as 'const' in C#, and essentially the compiler will
// do a drag/drop replacement of the literal value in the actual text document.

[<Literal>]
let displayColsIndexedBy = 1
[<Literal>] 
let firstColIndex = 0
[<Literal>] 
let lastColIndex = 6

let firstDisplayCol = firstColIndex + displayColsIndexedBy
let lastDisplayCol = lastColIndex + displayColsIndexedBy
let numCols = lastColIndex + 1

[<Literal>]
let firstRowIndex = 0
[<Literal>]
let lastRowIndex = 5

let numRows = lastRowIndex + 1

[<Literal>]
let xChip = 'X'
[<Literal>]
let oChip = 'O'
[<Literal>]
let emptySlot = '.'

let emptyBoard:Board = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

[<Literal>]
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

let showPlayerName player =
    match player with
    | Player1 -> "Player 1"
    | Player2 -> "Player 2"

let showGameOutcome status =
    match status with
    | Tie ->
        "The game ended in a tie!"
    | Win player ->
        showPlayerName player |> sprintf "%s won the game!"

let showBoardSlot (slot : BoardSlot) =
    match slot with
    | ChipType Player1 -> xChip
    | ChipType Player2 -> oChip
    | Empty -> emptySlot

let showBoard (board:Board) =
    Array2D.map showBoardSlot board
    |> format2dArray boardDisplayFormat
