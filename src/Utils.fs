module Utils

let rec mapFormatStringToOutputHelper
    (formatStr:char list)
    (replaceWith:char list)
    (outStr:char list) =
    match formatStr with
    | formatHead :: formatRemaining -> 
        match replaceWith with
        | replaceWithHead :: replaceWithRemaining -> 
        mapFormatStringToOutputHelper
            formatRemaining
            replaceWithRemaining
            (outStr @ [
                match formatHead with
                | '@' -> replaceWithHead
                | _ -> formatHead
            ])
        | [] -> outStr
        // FIX: here, it should keep going instead of ending
        // if we run out of replaceWiths
        // also, for some reason, it says we never have any replaceWiths
    | [] -> outStr

/// replaces every "@" in the format string with the
/// next array element
let format2dArray (formatStr:string) (arr:char[,]) =
    let replaceWith = arr |> Seq.cast<char> |> Seq.toList
    let formatStr' = formatStr.ToCharArray() |> Seq.cast<char> |> Seq.toList
    mapFormatStringToOutputHelper formatStr' replaceWith []
    |> Array.ofList
    |> System.String
    
