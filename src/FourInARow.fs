module FourInARow
open OOWrappers

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardColumn = int
type Board = BoardSlot array
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
let emptyBoard = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

let applyMove board player boardColumn =
    // TODO apply the move
    board

let showBoard (board:Board) =
    printfn "%A" board
    // TODO make it pretty
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

let inputPlayerMoveGetter (board:Board) =
    firstCol

let randomPlayerMoveGetter (board:Board) =
    randomNextInt firstCol (lastCol + 1)

let getPlayerMoveGetterPvAI player :PlayerMoveGetter =
    match player with
    | Player1 -> fun board -> 1
    | Player2 -> fun board -> 1

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board) =
    match getBoardStatus board with
    | StillGoing -> 
        getMoveGetter whosTurn
        |> takeTurn board whosTurn 
        |> gameLoop getMoveGetter (getNextTurn whosTurn)
    | GameOver status -> board
