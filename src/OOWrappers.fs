module OOWrappers
open System

let random = new Random();
let randomNextInt inclusiveMin uninclusiveMax = random.Next(inclusiveMin, uninclusiveMax)

let readConsoleLine () = Console.ReadLine()

let stringIndexOf (string:string) (key:char) =
    string.IndexOf(key)