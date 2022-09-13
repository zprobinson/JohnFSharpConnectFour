module PlayerMoveGetting
open Domain
open Utils
open OOWrappers

let displayColToColIndex displayCol =
    displayCol - displayColsIndexedBy

// TODO: abstract the request printing
let rec inputPlayerMoveGetter (player : Player) (board : Board) =
    printf "%s: Enter a column to play your next chip (%i-%i) >>> "
        (showPlayerName player) firstDisplayCol lastDisplayCol
    match tryReadConsoleInt () with
    | None -> 
        printfn "That was not a number"
        inputPlayerMoveGetter player board
    | Some int -> displayColToColIndex int

// I know this function isn't useful, but in case you didn't know...
// You can create a tuple and pipe multiple arguments into a function
// by using additional vertical bars in your piping operator.
// Pretty sweet, huh?
let printPipeTest player =
    let playerName = showPlayerName player
    (playerName, firstDisplayCol, lastDisplayCol)
    |||> printf "%s: Enter a column to play your next chip (%i-%i) >>> "

let randomPlayerMoveGetter (player : Player) (board : Board) =
    let colPlayed = randomNextInt firstDisplayCol (lastDisplayCol + 1)
    printf "%s: Enter a column to play your next chip (%i-%i) >>> %i\n"
        (showPlayerName player) firstDisplayCol lastDisplayCol colPlayed
    colPlayed |> displayColToColIndex

let playerVsRandGetPlayerMoveGetter player : PlayerMoveGetter =
    match player with
    | Player1 -> inputPlayerMoveGetter player
    | Player2 -> randomPlayerMoveGetter player

let isColumnFull (board : Board) col =
    board[0, col] = Empty |> not

// You can create your own operators in F#!!
// Lets create an "inclusive between" operator
// 'inline' is a performance gain keyword for functions
// It is pretty frequently used with custom operators (cuz they are used a lot)
let inline (>=<) x (min, max) = (x >= min) && (x <= max)

// Lets use it in an example function
// Oh yeah, you can define functions as sentences using ``___``
let ``is between 0 and 10`` x =
    if x >=< (0, 10) then
        sprintf "%i was in between 0 and 10 (inclusive)!"
    else
        sprintf "%i was not in between 0 and 10 (inclusive) :("

// Doesn't this code look wack when it is called?
// It is SUPER useful when writing unit tests though.
// You can be very descriptive in your unit test function declaration.
// Also good for creating build scripts in F# using FAKE instead of yaml.
let check15 = 15 |> ``is between 0 and 10``
let check8 = 8 |> ``is between 0 and 10``

let rec getValidatedMove (moveGetter : PlayerMoveGetter) (board : Board) =
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
