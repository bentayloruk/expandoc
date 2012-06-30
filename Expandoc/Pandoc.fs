[<RequireQualifiedAccess>]
module Pandoc

open System.IO
open YamlDotNet.RepresentationModel
open System
open System.Diagnostics
open IO
open Arg

///Looks in program file folder for Pandoc.exe
let findPandoc () =
    let pandocFolder = "Pandoc"
    let progFilePaths = [
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), pandocFolder); 
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), pandocFolder); 
        ]
    progFilePaths 
    |> Seq.map (fun path -> searchFolder ["pandoc.exe"] path SearchAllBelow) 
    |> Seq.concat |> List.ofSeq |> function | [] -> "" | h::_ -> printfn "Found pandoc here %s" h; h


//Runs pandoc for filePath to html.  Drops in same location.
let toHtml pandocPath (additionalArgs:ExpandocArg list) (input:TextReader) =
    use p = new Process()
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError <- true
    p.StartInfo.FileName <- pandocPath
    //p.StartInfo.Arguments <- sprintf " -o \"%s\"" outPath
    let arguments = 
        "-toc -t html" + 
            if additionalArgs.Length > 0 then
                " " + 
                String.Join(" ", additionalArgs |> List.map (fun arg -> arg.ToString()))
            else ""
        
    p.StartInfo.Arguments <- arguments
    p.StartInfo.RedirectStandardInput <- true
    try
        //TODO this reading is quick and dirty.  Need to redo this.
        p.Start() |> ignore
        p.StandardInput.Write(input.ReadToEnd())
        p.StandardInput.Close()
        let output = p.StandardOutput.ReadToEnd()
        let failText = p.StandardError.ReadToEnd()
        p.WaitForExit()//TODO add back in some timeout facility like FAKE
        (p.ExitCode, failText, output)
    with
    | exn -> failwithf "Start of process %s failed. %s" p.StartInfo.FileName exn.Message


