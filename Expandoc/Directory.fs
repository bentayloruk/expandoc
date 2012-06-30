module Directory
open System.IO

//Returns a sequence of all sub-directories of path directory.  Does not recurse or include dirs that match filter. 
let rec seqDirsBelow path filter =
    if Directory.Exists(path) <> true then failwith <| sprintf "Path %s does not exist" path
    seq {
            for dir in Directory.EnumerateDirectories(path) do
            if filter dir <> true then
                yield! seqDirsBelow dir filter
                yield dir 
        }

let seqFilesIn dirs = 
    seq {
        for dir in dirs do
            yield! Directory.EnumerateFiles(dir)
            }

///Returns a sequence of all the files in the provided dirs.
let rec filesInDirs dirs =
    seq {
            for dir in dirs do
                yield! Directory.EnumerateFiles(dir)
        }

///Deletes all the child directories and files in path.
let cleanDirectory path =
    if Directory.Exists(path) then 
        for dir in Directory.GetDirectories(path) do
            Directory.Delete(dir, true)
        for file in Directory.GetFiles(path) do
            File.Delete(file)
    else ()

let ensureDir dir = 
    if Directory.Exists(dir) <> true 
        then Directory.CreateDirectory(dir) |> ignore

