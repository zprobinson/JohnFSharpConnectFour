[<RequireQualifiedAccess>]
module Option

// We can extend existing modules!
// This code comes from a fantastic nuget package
// (which I recommend you try out) called FSharpPlus
// Code here: https://github.com/fsprojects/FSharpPlus/blob/master/src/FSharpPlus/Extensions/Option.fs#L50-50

let protect (f: 'T -> 'U) x =
    try
        Some (f x)
    with _ -> None