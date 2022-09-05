module BoardManipulation
open Domain
open Utils

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

let rec pickListFrom2dArray (continueWithThis) (getNextPosition) (arr:'a[,]) ((row, col):int * int) (accum:'a list) =
    match continueWithThis row col with
    | false -> accum
    | true -> pickListFrom2dArray continueWithThis getNextPosition arr (getNextPosition (row, col)) (arr[row, col] :: accum) 

let pickRow (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> col <= lastColIndex)
        (fun (row, col) -> (row, col + 1))
        board
        (row,0)
        []

let pickColumn (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex)
        (fun (row, col) -> (row + 1, col))
        board
        (0,col)
        []

let getUpDiagonalStart (rowStart,colStart) =
    let distanceToEdge = min (lastRowIndex - rowStart) (colStart - firstColIndex)
    (rowStart + distanceToEdge, colStart - distanceToEdge)

let pickUpDiagonal (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row >= firstRowIndex && col <= lastColIndex)
        (fun (row, col) -> (row - 1, col + 1))
        board
        (getUpDiagonalStart (row, col))
        []

let getDownDiagonalStart (rowStart,colStart) =
    let distanceToEdge = min (rowStart - firstRowIndex) (colStart - firstColIndex)
    (rowStart - distanceToEdge, colStart - distanceToEdge)

let pickDownDiagonal (board:Board) (row:BoardRow) (col:BoardColumn) =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex && col <= lastColIndex)
        (fun (row, col) -> (row + 1, col + 1))
        board
        (getDownDiagonalStart (row, col))
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
