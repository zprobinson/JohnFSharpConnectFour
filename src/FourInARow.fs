module FourInARow
open OOWrappers
open Utils
open Domain

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

let getNextTurn (thisTurn:Player) =
    match thisTurn with
    | Player1 -> Player2
    | Player2 -> Player1

let rec inputPlayerMoveGetter (player:Player) (board:Board) =
    printf "%A: Enter a column to play your next chip (%i-%i) >>> " player firstDisplayCol lastDisplayCol
    match tryReadConsoleInt () with
    | None -> 
        printfn "That was not a number"
        inputPlayerMoveGetter player board
    | Some int -> int - 1

let randomPlayerMoveGetter (player:Player) (board:Board) =
    let colPlayed = randomNextInt firstDisplayCol (lastDisplayCol + 1)
    printf "%A: Enter a column to play your next chip (%i-%i) >>> %i\n"
        player firstDisplayCol lastDisplayCol colPlayed
    colPlayed |> (+) -1

let playerVsRandGetPlayerMoveGetter player :PlayerMoveGetter =
    match player with
    | Player1 -> inputPlayerMoveGetter player
    | Player2 -> randomPlayerMoveGetter player

let isColumnFull (board:Board) col =
    board[0, col] = Empty |> not

let rec getValidatedMove (moveGetter:PlayerMoveGetter) (board:Board) =
    let moveCol = moveGetter board
    match moveCol >= firstColIndex && moveCol <= lastColIndex with
    | false ->
        printfn "That is not a valid column"
        getValidatedMove moveGetter board
    | true -> 
        match isColumnFull board moveCol with
        | true ->
            printfn "That column is full"
            getValidatedMove moveGetter board
        | false -> moveCol

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board) =
    let moveCol = getValidatedMove (getMoveGetter whosTurn) board
    let moveRow = findLowestEmptyRowInCol board moveCol
    let board' = insertChip board whosTurn moveRow moveCol
    let boardStatus = boardStatusAfterMove board' whosTurn moveRow moveCol
    showBoard board' |> ignore
    match boardStatus with
    | StillGoing -> gameLoop getMoveGetter (getNextTurn whosTurn) board'
    | GameOver status -> status

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

emptyBoard
|> showBoard
|> gameLoop playerVsRandGetPlayerMoveGetter Player1
|> showGameOutcome
|> printfn "%s"
