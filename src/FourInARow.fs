module FourInARow
open OOWrappers
open Utils

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardColumn = int
type Board = BoardSlot [,]
type GameOverStatus =
    | Player1Win
    | Player2Win
    | Tie
type BoardStatus = 
    | GameOver of GameOverStatus
    | StillGoing
type PlayerMoveGetter = Board -> BoardColumn
type GetPlayerMoveGetter = Player -> PlayerMoveGetter
type ApplyMove = Player -> BoardColumn -> Board -> Board
type DoTurn = Player -> PlayerMoveGetter -> Board -> Board

let firstCol = 1
let lastCol = 7
let numCols = lastCol
let firstRow = 1
let lastRow = 6
let numRows = lastRow

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

let applyMove board player boardColumn =
    // TODO apply the move
    board

let showBoardSlot (slot:BoardSlot) =
    match slot with
    | ChipType Player1 -> xChip
    | ChipType Player2 -> oChip
    | Empty -> emptySlot

let showBoard (board:Board) =
    Array2D.map showBoardSlot board
    |> format2dArray boardDisplayFormat
    |> printfn "%s\n"
    board

let getBoardStatus (board:Board) =
    StillGoing
    // TODO get the board status like win or tie

let takeTurn
    (board:Board)
    (player:Player)
    (getPlayerMove:PlayerMoveGetter) =
    getPlayerMove board
    |> applyMove board player
    |> showBoard

let getNextTurn (thisTurn:Player) =
    match thisTurn with
    | Player1 -> Player2
    | Player2 -> Player1

let inputPlayerMoveGetter (player:Player) (board:Board) =
    printf "%A: Enter a column to play your next chip (%i-%i) >>> " player firstCol lastCol
    readConsoleLine () |> int // TODO add validation

let randomPlayerMoveGetter (player:Player) (board:Board) =
    let colPlayed = randomNextInt firstCol (lastCol + 1)
    printf "%A: Enter a column to play your next chip (%i-%i) >>> %i\n"
        player firstCol lastCol colPlayed
    colPlayed

let playerVsRandGetPlayerMoveGetter player :PlayerMoveGetter =
    match player with
    | Player1 -> inputPlayerMoveGetter player
    | Player2 -> randomPlayerMoveGetter player

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board) =
    match getBoardStatus board with
    | StillGoing -> 
        getMoveGetter whosTurn
        |> takeTurn board whosTurn 
        |> gameLoop getMoveGetter (getNextTurn whosTurn)
    | GameOver status ->
        printfn "Game Over! Status: %A" status

gameLoop playerVsRandGetPlayerMoveGetter Player1 emptyBoard
