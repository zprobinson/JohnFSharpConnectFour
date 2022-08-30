module FourInARow

type ChipType = Player1 | Player2
type BoardSlot = ChipType of ChipType | Empty
type BoardColumn = int
type Board = BoardSlot array
type GameOverStatus =
    | Player1Win
    | Player2Win
    | Tie
type BoardStatus = 
    | GameOver of GameOverStatus
    | StillGoing
type GetPlayerMove = Board -> BoardColumn
type ApplyMove = ChipType -> BoardColumn -> Board -> Board
type DoTurn = ChipType -> GetPlayerMove -> Board -> Board

let numRows = 6
let numCols = 7
let emptyBoard = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

let applyMove chipType boardColumn board =
    board

let doTurn chipType getPlayerMove board =
    let playerMove = getPlayerMove board
    board |> applyMove chipType playerMove

let showTurn (board:Board) =
    printfn "%A" board
    board

let isGameOver (board:Board) =
    false

let rec gameLoop
    (getP1Move:GetPlayerMove)
    (getP2Move:GetPlayerMove)
    (board:Board) =
    board
    |> doTurn Player1 getP1Move
    |> showTurn
    |> doTurn Player2 getP2Move
    |> showTurn
    |> gameLoop getP1Move getP2Move
