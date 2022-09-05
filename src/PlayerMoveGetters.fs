module PlayerMoveGetters
open Domain
open Utils
open OOWrappers

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
