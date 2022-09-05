module Utils
open System
open OOWrappers

let rec mapFormatStringToOutputHelper
    (formatStr:char list)
    (replaceWith:char list)
    (outStr:char list) =
    match formatStr with
    | formatHead :: remainingFormat ->
        match replaceWith with
        | replaceWithHead :: remainingReplaceWith ->
            match formatHead with
            // TODO clean up some code repetition
            | '@' -> mapFormatStringToOutputHelper remainingFormat remainingReplaceWith (outStr @ [replaceWithHead])
            | _ -> mapFormatStringToOutputHelper remainingFormat replaceWith (outStr @ [formatHead])
        | [] -> mapFormatStringToOutputHelper remainingFormat replaceWith (outStr @ [formatHead])
    | [] -> outStr

/// replaces every "@" in the format string with the
/// next array element
let format2dArray (formatStr:string) (arr:char[,]) =
    let replaceWith = arr |> Seq.cast<char> |> Seq.toList
    let formatStr' = formatStr.ToCharArray() |> Seq.cast<char> |> Seq.toList
    mapFormatStringToOutputHelper formatStr' replaceWith []
    |> Array.ofList
    |> String

let tryParseInt str =
    try
        str |> int |> Some
    with :? FormatException ->
        None

let readConsoleInt () =
    match readConsoleLine () |> tryParseInt with
    | Some int -> Some int
    | None -> None
