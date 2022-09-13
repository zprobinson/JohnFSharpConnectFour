module OOWrappers
open System

let random = new Random();
let randomNextInt inclusiveMin exclusiveMax =
    random.Next(inclusiveMin, exclusiveMax)

// Just like in math, if you have the same value on both sides,
// You can subtract it from both sides and everything stays the same!
// Here we had unit "()" on both sides, so we can get rid of it and it means the same thing.
// Writing functions without specifying the parameters is called writing them "point-free".
//  (unit is a parameter)
// Point-free code can be elegant, but also confusing.
// A fun challenge is to try and write an F# Fibonacci function as point-free as possible.
let readConsoleLine = Console.ReadLine

// I try to avoid using parens when not needed when making function calls
// I prefer using the `var : type` declaration over `var:type` or `var: type`
// You could probably install an F# formatter like `Fantomas` to handle formatting automatically.
// Additionally you can see F# code formatting guidelines here: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting
// FSharp Style Guide here: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/
let indexOf (string : string) (key : char) =
    string.IndexOf key
