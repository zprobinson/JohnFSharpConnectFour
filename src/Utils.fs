module Utils
open System
open OOWrappers

let rec mapFormatStringToOutputHelper
    (formatChars:char list)
    (replaceWith:char list)
    (formatted:char list) =
    match formatChars with
    | [] -> formatted
    | formatHead :: remainingFormat ->
        match replaceWith with
        | [] -> mapFormatStringToOutputHelper remainingFormat replaceWith (formatted @ [formatHead])
        | replaceWithHead :: remainingReplaceWith ->
            match formatHead with
            // TODO clean up some code repetition
            | '@' -> mapFormatStringToOutputHelper remainingFormat remainingReplaceWith (formatted @ [replaceWithHead])
            | _ -> mapFormatStringToOutputHelper remainingFormat replaceWith (formatted @ [formatHead])

/// replaces every "@" in the format string with
/// array elements, in order
let format2dArray (formatStr:string) (arr:char[,]) =
    let replaceWith = arr |> Seq.cast<char> |> Seq.toList
    let formatChars = formatStr.ToCharArray() |> Seq.cast<char> |> Seq.toList
    mapFormatStringToOutputHelper formatChars replaceWith []
    |> Array.ofList
    |> String

let tryParseInt str =
    try
        str |> int |> Some
    with :? FormatException ->
        None

let tryReadConsoleInt () =
    readConsoleLine () |> tryParseInt
