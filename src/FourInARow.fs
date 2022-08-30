module FourInARow

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

let numRows = 6
let numCols = 7
let emptyBoard = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

let applyMove board player boardColumn =
    board

let showBoard (board:Board) =
    printfn "%A" board
    board

let getBoardStatus (board:Board) =
    StillGoing

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

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board)
    =
    match getBoardStatus board with
    | StillGoing -> 
        getMoveGetter whosTurn
        |> takeTurn board whosTurn 
        |> gameLoop getMoveGetter (getNextTurn whosTurn)
    | GameOver status -> board
