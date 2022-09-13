module OOWrappers
open System

let random = new Random();
let randomNextInt inclusiveMin exclusiveMax =
    random.Next(inclusiveMin, exclusiveMax)

let readConsoleLine () = Console.ReadLine()

// I try to avoid using parens when not needed when making function calls
// I prefer using the `var : type` declaration over `var:type` or `var: type`
// You could probably install an F# formatter like `Fantomas` to handle formatting automatically.
// Additionally you can see F# code formatting guidelines here: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting
// FSharp Style Guide here: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/
let indexOf (string : string) (key : char) =
    string.IndexOf key
