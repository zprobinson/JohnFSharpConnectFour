module FourInARow
open OOWrappers
open Utils

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardColumn = int
type BoardRow = int
type Board = BoardSlot [,]
type GameOverStatus =
    | Win of Player
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

let rec findLowestEmptyRowInColHelper (board:Board) col row =
    match row > (lastRow - 1) with
    | true -> row - 1
    | false ->
        match board[row, col] with
        | ChipType _ -> row - 1
        | Empty -> findLowestEmptyRowInColHelper board col (row + 1)

let findLowestEmptyRowInCol board boardColumn =
    findLowestEmptyRowInColHelper board boardColumn 0

let getMapping (player:Player) colToInsert rowToInsert =
    fun row col (existingSlot:BoardSlot) -> 
        match row = rowToInsert && col = colToInsert with
        | true -> ChipType player
        | false -> existingSlot

let insertChip (board:Board) (player:Player) (row:BoardRow) (col:BoardColumn) =
    getMapping player col row
    |> Array2D.mapi
    <| board

let showBoardSlot (slot:BoardSlot) =
    match slot with
    | ChipType Player1 -> xChip
    | ChipType Player2 -> oChip
    | Empty -> emptySlot

let showBoard (board:Board) =
    Array2D.map showBoardSlot board
    |> format2dArray boardDisplayFormat
    |> printfn "\n%s\n\n"
    board

let rec hasFourConsecutiveHelper (list:BoardSlot list) (checkFor:Player) (numConsecutive:int) =
    match numConsecutive with
    | 4 -> true
    | _ ->
        match list with
        | [] -> false
        | head :: rest ->
            match head = ChipType checkFor with
            | true -> hasFourConsecutiveHelper rest checkFor (numConsecutive + 1)
            | false -> hasFourConsecutiveHelper rest checkFor 0

let hasFourConsecutive (list:BoardSlot list) (checkFor:Player) =
    hasFourConsecutiveHelper list checkFor 0

let doesMoveCreateWin (board:Board) (player:Player) (row:BoardRow) (col:BoardColumn) =
    false // TODO implement

let rec isBoardFullHelper (board:Board) colIndex =
    match board[0, colIndex] with
    | Empty -> false
    | ChipType player ->
        match colIndex + 1 = lastCol with
        | true -> true
        | false -> isBoardFullHelper board (colIndex + 1)

let isBoardFull (board:Board) =
    isBoardFullHelper board 0

let boardStatusAfterMove (board:Board) (player:Player) (row:BoardRow) (col:BoardColumn) :BoardStatus =
    match doesMoveCreateWin board player row col with
    | true -> GameOver (Win player)
    | false ->
        match isBoardFull board with
        | true -> GameOver Tie
        | false -> StillGoing

let getNextTurn (thisTurn:Player) =
    match thisTurn with
    | Player1 -> Player2
    | Player2 -> Player1

let inputPlayerMoveGetter (player:Player) (board:Board) =
    printf "%A: Enter a column to play your next chip (%i-%i) >>> " player firstCol lastCol
    readConsoleLine () |> int |> (+) -1 // TODO add validation

let randomPlayerMoveGetter (player:Player) (board:Board) =
    let colPlayed = randomNextInt firstCol (lastCol + 1)
    printf "%A: Enter a column to play your next chip (%i-%i) >>> %i\n"
        player firstCol lastCol colPlayed
    colPlayed |> (+) -1

let playerVsRandGetPlayerMoveGetter player :PlayerMoveGetter =
    match player with
    | Player1 -> inputPlayerMoveGetter player
    | Player2 -> randomPlayerMoveGetter player

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board) =
    let moveCol = getMoveGetter whosTurn board
    let moveRow = findLowestEmptyRowInCol board moveCol
    let board' = insertChip board whosTurn moveRow moveCol
    let boardStatus = boardStatusAfterMove board' whosTurn moveRow moveCol
    showBoard board' |> ignore
    match boardStatus with
    | StillGoing -> gameLoop getMoveGetter (getNextTurn whosTurn) board'
    | GameOver status -> false // TODO end the game

gameLoop playerVsRandGetPlayerMoveGetter Player1 emptyBoard |> ignore
