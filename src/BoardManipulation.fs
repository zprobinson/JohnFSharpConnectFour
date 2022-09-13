module BoardManipulation
open Domain
open Utils

let rec findLowestEmptyRowInColHelper (board : Board) col row =
    match row > (lastRowIndex) with
    | true -> row - 1
    | false ->
        match board[row, col] with
        | ChipType _ -> row - 1
        | Empty -> findLowestEmptyRowInColHelper board col (row + 1)

let findLowestEmptyRowInCol board boardColumn =
    findLowestEmptyRowInColHelper board boardColumn 0

let getInsertChipMapping player rowToInsert colToInsert =
    fun row col existingSlot -> 
        match row = rowToInsert && col = colToInsert with
        | true -> ChipType player
        | false -> existingSlot

let insertChip board player row col =
    getInsertChipMapping player row col
    |> Array2D.mapi
    <| board

let rec hasFourConsecutiveHelper list checkFor numConsecutive =
    match numConsecutive with
    | 4 -> true
    | _ ->
        match list with
        | [] -> false
        | head :: rest ->
            match head = ChipType checkFor with
            | true -> hasFourConsecutiveHelper rest checkFor (numConsecutive + 1)
            | false -> hasFourConsecutiveHelper rest checkFor 0

// TODO: refactor to be more generic
let hasFourConsecutive checkFor list =
    hasFourConsecutiveHelper list checkFor 0

// You aren't using the "col" parameter in this function
// but i don't want to remove and create compiler errors.
let pickRow (board : Board) row col =
    pickListFrom2dArray
        (fun _ col -> col <= lastColIndex)
        (fun (row, col) -> (row, col + 1))
        board
        (row,0)
        []

// You aren't using the "row" parameter in this function
// but I don't wanna remove and create errors.
let pickColumn (board : Board) row col =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex)
        (fun (row, col) -> (row + 1, col))
        board
        (0,col)
        []

let getUpDiagonalStart (rowStart, colStart) =
    let distanceToEdge = min (lastRowIndex - rowStart) (colStart - firstColIndex)
    (rowStart + distanceToEdge, colStart - distanceToEdge)

let pickUpDiagonal (board : Board) row col =
    pickListFrom2dArray
        (fun row col -> row >= firstRowIndex && col <= lastColIndex)
        (fun (row, col) -> (row - 1, col + 1))
        board
        (getUpDiagonalStart (row, col))
        []

let getDownDiagonalStart (rowStart, colStart) =
    let distanceToEdge = min (rowStart - firstRowIndex) (colStart - firstColIndex)
    (rowStart - distanceToEdge, colStart - distanceToEdge)

let pickDownDiagonal (board : Board) row col =
    pickListFrom2dArray
        (fun row col -> row <= lastRowIndex && col <= lastColIndex)
        (fun (row, col) -> (row + 1, col + 1))
        board
        (getDownDiagonalStart (row, col))
        []


let getPossibleWinSequences board row col =
    [
        pickRow
        pickColumn
        pickUpDiagonal
        pickDownDiagonal
    ]
    |> List.map (fun pickSequence -> pickSequence board row col)

let doesMoveCreateWin board player row col =
    getPossibleWinSequences board row col
    |> List.exists (hasFourConsecutive player)

let rec isBoardFullHelper (board : Board) col =
    match board[0, col] with
    | Empty -> false
    // I like to use the discard char if I'm not using a param.
    // Makes it official
    | ChipType _ ->
        match col + 1 = lastDisplayCol with
        | true -> true
        | false -> isBoardFullHelper board (col + 1)

// Function parameter positioning is super important in F# 
//  (and in functional programming languages in general that support currying).
// If you are intentional about how you position your parameters,
// You can use "partial application" to make your code a lot
// more concise and clean.
let flip f a b = f b a
// Now this function takes the "col" first and the "board" 2nd
let isBoardFullHelper' = flip isBoardFullHelper
// Notice how we can get rid of the extra parameter here
// Now "isBoardFull'" is of type (Board -> bool) just like the function below.
// But it is cleaner because we organized the parameters differently
// which allowed us to use partial application.
let isBoardFull' = isBoardFullHelper' 0

let isBoardFull board =
    isBoardFullHelper board 0

let boardStatusAfterMove board player row col =
    match doesMoveCreateWin board player row col with
    | true -> GameOver (Win player)
    | false ->
        match isBoardFull board with
        | true -> GameOver Tie
        | false -> StillGoing
