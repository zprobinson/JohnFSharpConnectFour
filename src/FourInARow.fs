﻿module FourInARow
open Domain
open PlayerMoveGetting
open BoardManipulation

let printBoardInline (board:Board) =
    showBoard board
    |> printfn "\n%s\n\n"
    board

let getNextTurn (thisTurn:Player) =
    match thisTurn with
    | Player1 -> Player2
    | Player2 -> Player1

let rec gameLoop
    (getMoveGetter:GetPlayerMoveGetter)
    (whosTurn:Player)
    (board:Board) =
    let moveCol = getValidatedMove (getMoveGetter whosTurn) board
    let moveRow = findLowestEmptyRowInCol board moveCol
    let board' = insertChip board whosTurn moveRow moveCol
    let boardStatus = boardStatusAfterMove board' whosTurn moveRow moveCol
    printBoardInline board' |> ignore
    match boardStatus with
    | StillGoing ->
        gameLoop getMoveGetter (getNextTurn whosTurn) board'
    | GameOver status ->
        status

emptyBoard
|> printBoardInline
|> gameLoop playerVsRandGetPlayerMoveGetter Player1
|> showGameOutcome
|> printfn "%s"
