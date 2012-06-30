module Path

open System.IO
open System.Reflection

//Gets the filename from the path.
let fileName path =
    Path.GetFileName path

let combine pathLeft pathRight =
    Path.Combine (pathLeft, pathRight)

let processExeDir () =
    Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)

