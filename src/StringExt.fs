[<RequireQualifiedAccess>]
module StringExt

// Cool function composition!
// Notice how the types line up, which allow us to build
// the function without ever considering the incoming data!
// Array.ofList :: T list -> T[]
// String       :: char[] -> string
// ofList       :: char list -> string
let ofList = Array.ofList >> System.String