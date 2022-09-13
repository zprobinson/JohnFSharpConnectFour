module Utils
open System
open OOWrappers

// I ain't touchin it, good luck fam.
let rec mapFormatStringToOutputHelper
    (formatChars : char list)
    (replaceWith : char list)
    (formatted : char list) =
    match formatChars with
    | [] -> formatted
    | head :: tail ->
        match replaceWith with
        | [] ->
            mapFormatStringToOutputHelper
                tail
                replaceWith
                (formatted @ [head])
        | replaceWithHead :: remainingReplaceWith ->
            match head with
            | '@' ->
                mapFormatStringToOutputHelper
                    tail
                    remainingReplaceWith
                    (formatted @ [replaceWithHead])
            | _ ->
                mapFormatStringToOutputHelper
                    tail
                    replaceWith
                    (formatted @ [head])

/// replaces every "@" in the format string with
/// array elements, in order
let format2dArray (formatStr:string) (arr:char[,]) =
    mapFormatStringToOutputHelper
        (formatStr.ToCharArray() |> Seq.cast<char> |> Seq.toList)
        (arr |> Seq.cast<char> |> Seq.toList)
        []
    |> Array.ofList
    |> String

// Can more concisely get to the char[] by using List.ofArray
// Also includes a helper method call from StringExt module.
let format2dArray' (formatStr : string) (arr : char[,]) =
    let output =
        mapFormatStringToOutputHelper
            (formatStr.ToCharArray() |> List.ofArray)
            (arr |> Seq.cast<char> |> Seq.toList)
            []

    StringExt.ofList output

let tryParseInt str =
    try
        str |> int |> Some
    with :? FormatException ->
        None

// We can build a protective parsing function through our custom Option.protect
// Use this if we don't care about the error because the exception is discarded (like above)
let tryParseInt' = Option.protect int

// We have another type called 'Result' which has an Ok and Error.
// This is an Either type that allows two generics.
// This way, if we care about the Exception, we can use it later.
// It will be used in a match statement like 
// ```match result with | Ok int -> _ | Error exn -> _```
let tryParseInt'' str = 
    let resultInt = int >> Ok
    try
        resultInt str
    with
    | :? FormatException as exn -> Error (exn :> exn)
    | _ as uhoh -> Error uhoh

let tryReadConsoleInt () =
    readConsoleLine () |> tryParseInt

// You can use the apostrophe (') as a valid character
// when declaring identifiers. Normally it is used for
// related but alternate implementations.
// Again here we can use function composition piping if we want!
let tryReadConsoleInt' = readConsoleLine >> tryParseInt

// Learn a bit about tail recursion in F# functions.
// Link: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword#tail-recursion
let rec pickListFrom2dArray
    continueWithThis
    getNextPosition
    (arr : 'a[,])
    ((row, col) : int * int)
    (accum : 'a list) =
    match continueWithThis row col with
    | false -> accum
    | true ->
        pickListFrom2dArray
            continueWithThis
            getNextPosition
            arr
            (getNextPosition (row, col))
            (arr[row, col] :: accum)