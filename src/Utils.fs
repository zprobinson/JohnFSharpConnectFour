module Utils
open System
open OOWrappers

let rec mapFormatStringToOutputHelper
    (formatChars:char list)
    (replaceWith:char list)
    (formatted:char list) =
    match formatChars with
    | [] ->
        formatted
    | formatHead :: remainingFormat ->
        match replaceWith with
        | [] ->
            mapFormatStringToOutputHelper
                remainingFormat
                replaceWith
                (formatted @ [formatHead])
        | replaceWithHead :: remainingReplaceWith ->
            match formatHead with
            | '@' ->
                mapFormatStringToOutputHelper
                    remainingFormat
                    remainingReplaceWith
                    (formatted @ [replaceWithHead])
            | _ ->
                mapFormatStringToOutputHelper
                    remainingFormat
                    replaceWith
                    (formatted @ [formatHead])

/// replaces every "@" in the format string with
/// array elements, in order
let format2dArray (formatStr:string) (arr:char[,]) =
    mapFormatStringToOutputHelper
        (formatStr.ToCharArray() |> Seq.cast<char> |> Seq.toList)
        (arr |> Seq.cast<char> |> Seq.toList)
        []
    |> Array.ofList
    |> String

let tryParseInt str =
    try
        str |> int |> Some
    with :? FormatException ->
        None

let tryReadConsoleInt () =
    readConsoleLine () |> tryParseInt

let rec pickListFrom2dArray
    continueWithThis
    getNextPosition
    (arr:'a[,])
    ((row, col):int * int)
    (accum:'a list) =
    match continueWithThis row col with
    | false -> accum
    | true ->
        pickListFrom2dArray
            continueWithThis
            getNextPosition
            arr
            (getNextPosition (row, col))
            (arr[row, col] :: accum)

