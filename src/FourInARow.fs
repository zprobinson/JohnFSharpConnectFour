module FourInARow
open OOWrappers
open Utils

type Player = Player1 | Player2
type BoardSlot = ChipType of Player | Empty
type BoardColumn = int
type BoardRow = int
type Board = BoardSlot [,]
type BoardPosition = BoardRow * BoardColumn
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

let displayColIndexedBy = 1
let firstColIndex = 0
let lastColIndex = 6
let firstDisplayCol = firstColIndex + displayColIndexedBy
let lastDisplayCol = lastColIndex + displayColIndexedBy
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

let rec findLowestEmptyRowInColHelper (board:Board) col row =
    match row > (lastRowIndex) with
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

let hasFourConsecutive (checkFor:Player) (list:BoardSlot list) =
    hasFourConsecutiveHelper list checkFor 0

let rec pickListFrom2dArray (continueWithThis) (getNextPosition) (arr:'a[,]) ((x, y):int * int) (accum:'a list) =
    match continueWithThis x y with
    | false -> accum
    | true -> pickListFrom2dArray continueWithThis getNextPosition arr (getNextPosition (x, y)) (arr[x, y] :: accum)

let pickRow (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> col <= lastColIndex)
        (fun (x,y) -> (x + 1,y))
        board
        (row,0)
        []

let pickColumn (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex)
        (fun (x,y) -> (x,y + 1))
        board
        (0,col)
        []

let pickUpDiagonal (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row >= firstRowIndex && col <= lastColIndex)
        (fun (x,y) -> (x + 1,y + 1))
        board
        (rowStart,colStart) // TODO
        []

let pickDownDiagonal (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex && col <= lastColIndex)
        (fun (x,y) -> (x - 1,y - 1))
        board
        (rowStart,colStart) // TODO
        []

let getPossibleWinSequences (board:Board) (row:BoardRow) (col:BoardColumn) :BoardSlot list list =
    [
        pickRow
        pickColumn
        pickUpDiagonal
        pickDownDiagonal
    ]
    |> List.map (fun pickSequence -> pickSequence board row col)

let doesMoveCreateWin (board:Board) (player:Player) (row:BoardRow) (col:BoardColumn) =
    getPossibleWinSequences board row col
    |> List.exists (hasFourConsecutive player)

let rec isBoardFullHelper (board:Board) col =
    match board[0, col] with
    | Empty -> false
    | ChipType player ->
        match col + 1 = lastDisplayCol with
        | true -> true
        | false -> isBoardFullHelper board (col + 1)

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
    printf "%A: Enter a column to play your next chip (%i-%i) >>> " player firstDisplayCol lastDisplayCol
    readConsoleLine () |> int |> (+) -1 // TODO add validation

let randomPlayerMoveGetter (player:Player) (board:Board) =
    let colPlayed = randomNextInt firstDisplayCol (lastDisplayCol + 1)
    printf "%A: Enter a column to play your next chip (%i-%i) >>> %i\n"
        player firstDisplayCol lastDisplayCol colPlayed
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
