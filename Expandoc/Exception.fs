module Exception

let protect msg errReturn f =
    fun x ->
        try
            (true, f x)
        with
        | _ -> 
            printfn "Error protected for %s" msg
            (false, errReturn)
