[<AutoOpen>]
module List

///Returns None if list is empty, otherwise List.head.
let headOption = function
    | [] -> None
    | list -> Some(List.head list)

let headOrDefault def list =
    match list with
    | [] -> def 
    | list -> List.head list