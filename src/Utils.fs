module Utils
open System

///// "send through"
///// apply a function that returns unit, then return the input
// let (>>>) input fn = 
//     printfn input
//     input

/// replaces every "@" in the format string with the
/// next array element
let rec format2dArray (format:string) (arr:char[,]) =
    // TODO implement
    format
