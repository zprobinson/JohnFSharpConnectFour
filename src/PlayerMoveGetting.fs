module PlayerMoveGetting
open Domain
open Utils
open OOWrappers

let displayColToColIndex displayCol =
    displayCol - displayColsIndexedBy

// TODO: abstract the request printing
let rec inputPlayerMoveGetter (player:Player) (board:Board) =
    printf "%s: Enter a column to play your next chip (%i-%i) >>> "
        (showPlayerName player) firstDisplayCol lastDisplayCol
    match tryReadConsoleInt () with
    | None -> 
        printfn "That was not a number"
        inputPlayerMoveGetter player board
    | Some int -> displayColToColIndex int

let randomPlayerMoveGetter (player:Player) (board:Board) =
    let colPlayed = randomNextInt firstDisplayCol (lastDisplayCol + 1)
    printf "%s: Enter a column to play your next chip (%i-%i) >>> %i\n"
        (showPlayerName player) firstDisplayCol lastDisplayCol colPlayed
    colPlayed |> displayColToColIndex

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
