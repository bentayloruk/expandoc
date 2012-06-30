module IO

open System.IO
open System
open System.Diagnostics
open System.Text
open Directory

//Gets the paths of sub-directories in direcroty path.
let getDirectories path =
    Directory.GetDirectories(path)

//Open a file for read and apply a function (in the use scope).  Is this a bad idea?  What if the caller holds a ref to the reader?
let readFileCont path cont =
    use stream = File.Open(path, FileMode.Open, FileAccess.Read)
    use reader = new StreamReader(stream)
    cont reader

//Writes the text as a UTF8 text file at the given path.  Creates directories if required.
let writeTextFile (text:string) path =
    let dir = Path.GetDirectoryName(path)
    if Directory.Exists(dir) <> true then Directory.CreateDirectory(dir) |> ignore
    let bytes = Encoding.UTF8.GetBytes(text)
    use file = File.Open(path, FileMode.Create)
    file.Write(bytes, 0, bytes.Length)
    file.Close()

type FolderSearchOptions =
    | SearchRootOnly
    | SearchAllBelow 

let searchFolder patterns path option =
    let option = match option with | SearchAllBelow -> SearchOption.AllDirectories | SearchRootOnly -> SearchOption.TopDirectoryOnly
    try
        seq {
            for pattern in patterns do
                let files = Directory.GetFiles(path, pattern, option) 
                for file in files do
                    yield file
            }
    with
    | ex -> Seq.empty

let processDir path proc =
    let rec inner path =
        getDirectories path
        |> Array.map (fun s -> proc s; s)
        |> Array.iter inner
    inner path
    ()

let recurseCopy fromDir toDir policy =
    if Directory.Exists(fromDir) <> true then failwith "fromDir does not exist."
    ensureDir toDir
    let rec inner fromDir toDir =
        for file in Directory.GetFiles(fromDir) do
            if policy file then
                let destPath = Path.Combine(toDir, (Path.GetFileName(file)))
                File.Copy(file, destPath) |> ignore
        for childDir in Directory.GetDirectories(fromDir) do
            if policy childDir then
                let destPath = Path.Combine(toDir, DirectoryInfo(childDir).Name)
                ensureDir destPath
                inner childDir destPath
    inner fromDir toDir
    ()