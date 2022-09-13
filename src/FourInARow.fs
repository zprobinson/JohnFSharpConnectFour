module FourInARow
open Domain
open PlayerMoveGetting
open BoardManipulation

let printBoardInline board =
    showBoard board
    |> printfn "\n%s\n\n"
    board

let getNextTurn thisTurn =
    match thisTurn with
    | Player1 -> Player2
    | Player2 -> Player1

// Notice how even though we get rid of the type annotations here,
// F# can still determine all of the complex types, including the "getMoveGetter"!
// It is a personal preference on whether to annotate your functions,
// I tend toward trying to get away from annotating because it clutters the code and makes it less pretty.
// I can always check the types with my IDE!
let rec gameLoop getMoveGetter whosTurn board =
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

// Function composition is badass
let loop = gameLoop playerVsRandGetPlayerMoveGetter Player1
let run = printBoardInline >> loop >> showGameOutcome >> printfn "%s"
//run emptyBoard

emptyBoard
|> printBoardInline
|> gameLoop playerVsRandGetPlayerMoveGetter Player1
|> showGameOutcome
|> printfn "%s"
