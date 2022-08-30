module FourInARow

type ChipType = Player1 | Player2
type BoardSlot = Chip | Empty
type BoardColumn = int
type Board = BoardSlot array
type GetPlayerMove = Board -> BoardColumn
type ApplyMove = ChipType -> BoardColumn -> Board -> Board
type DoTurn = ChipType -> GetPlayerMove -> Board -> Board

let numRows = 6
let numCols = 7
let board = Array2D.init<BoardSlot> numRows numCols (fun x y -> Empty)

let applyMove:ApplyMove = fun chipType boardColumn board ->
    board

let doTurn:DoTurn = fun chipType getPlayerMove board ->
    let playerMove = getPlayerMove board
    board |> applyMove chipType playerMove

let showTurn board:Board =
    printfn "%A" board
    board

let rec gameLoopHelper
    (getP1Move:GetPlayerMove)
    (getP2Move:GetPlayerMove)
    (board:Board) =
    board
    |> doTurn Player1 getP1Move
    |> showTurn
    |> doTurn Player2 getP2Move
    |> showTurn

printfn "%A" board
