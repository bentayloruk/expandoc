///Contains the command line parsing junk.  The actual work is done in App.fs.
module Program

open System
open System.IO
open App
open Arg

let templateFolderName = "_templates"

[<EntryPoint>]
let main argv = 
    
    //Argument value cells.
    let docInPath = ref ""
    let docOutPath = ref ""
    let pandocPath = ref ""
    let scopes = ref [||] 
    let webServerPort = ref None 

    //See if we've been called with good args.
    let initErrCode =
        //Specify our usage.
        let usage = [ 
            ArgInfo("-pdi", ArgType.String (fun a -> docInPath := a), "Path for documents IN (e.g. markdown etc).")
            ArgInfo("-pdo", ArgType.String (fun a -> docOutPath := a), "Path for documents OUT (i.e. html out).")
            ArgInfo("-pp", ArgType.String (fun a -> pandocPath := a), "Path for Pandoc")
            ArgInfo("-serve", ArgType.Int(fun a -> webServerPort := Some(a)), "The port to serve the generated site from.")
            ArgInfo("-scopes", ArgType.String(fun a -> scopes := a.Split(',')), "TOC scopes.")
            ]
        let () = ArgParser.Parse usage

        //Search for pandoc, if not provided as arg.
        if !pandocPath = "" then pandocPath := Pandoc.findPandoc()

        //UGLY - Make this arg checking nicer.
        if !pandocPath = "" then
            printfn "Unable to find pandoc in program files and not specified as argument.  Please install Pandoc or provide path to location." 
            ArgParser.Usage usage; 1
        elif !docInPath = "" then
            printfn "You must specify the document in path." 
            ArgParser.Usage usage; 1
        elif !docOutPath = "" then
            printfn "You must specify the document out path." 
            ArgParser.Usage usage; 1
        elif (Directory.Exists(!docInPath)) <> true then
            printfn "Document source root directory does not exist.  Looked for %s."  !docInPath
            1
        else 
            printfn "\n\nWriting to %s.  \nOK to delete?  y/n?" !docOutPath
            let result = Console.ReadLine()
            if result.ToLowerInvariant() = "y" then 0 else 1
    
    if initErrCode = 0 then 
        runApp { defaultExpandocArgs with 
                    PandocPath = !pandocPath;
                    DocsInPath = !docInPath;
                    DocsOutPath = !docOutPath;
                    TemplatesPath = Path.Combine(!docInPath, templateFolderName)
                    HttpServerPort = !webServerPort
                    Scopes = !scopes
                    }
    else
        initErrCode
    