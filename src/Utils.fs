module Utils

let replaceFirst (str:string) (replaceWith:char) =
    str // TODO implement

let rec format2dArrayHelper (arr:char list) (format:string) =
    match arr with
    | head :: rest -> replaceFirst format head |> format2dArrayHelper rest
    | [] -> format

/// replaces every "@" in the format string with the
/// next array element
let format2dArray (format:string) (arr:char[,]) =
    let arr' = arr |> Seq.cast<char> |> Seq.toList
    format2dArrayHelper arr' format
