module OOWrappers
open System

let random = new Random();
let randomNextInt inclusiveMin exclusiveMax =
    random.Next(inclusiveMin, exclusiveMax)

let readConsoleLine () = Console.ReadLine()

let indexOf (string:string) (key:char) =
    string.IndexOf(key)
