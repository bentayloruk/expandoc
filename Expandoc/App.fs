///The main body of the app.
module App

open System
open System.IO
open System.Text
open IO
open Html
open HttpServer
open Directory
open YamlDotNet.RepresentationModel
open Arg
open Html
open Nustache
open FrontMatter

let emptyTocTitleText = "No Title"

type ExpandocArgs =
    { DocsInPath : string;
    DocsOutPath : string;
    TemplatesPath : string;
    HttpServerPort : option<int>;
    PandocPath : string;
    Scopes : array<string>
    }

let defaultExpandocArgs = 
    { DocsInPath = "";
    DocsOutPath = "";
    TemplatesPath = "";
    HttpServerPort = None;
    PandocPath = "";
    Scopes = [||];
    }

type FileInfo = { InPath:string; OutPath:string; RelativeOutPath:string;}
type TocEntry = { Text:string; Link:string}

let argsFromFrontMatter reader = match readFrontMatter reader with | Json(o) -> argsFromJson o | _ -> []

///Removes leading numbers from file and dir paths (e.g. 1234-file.html -> file.html).
let numberWang (path:string) =
    let parts = path.Split([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |])
    let nameWithoutNumberWang (name:string) =
        let wangIndex = name.IndexOf('-')
        if wangIndex = -1 then name else
            let maybeNumber = name.Substring(0, wangIndex)
            let (parsed, number) = System.Int32.TryParse(maybeNumber)
            if parsed = true then name.Substring(wangIndex+1) else name 
    let wangedParts = parts |> Seq.map nameWithoutNumberWang |> Array.ofSeq
    Path.Combine(wangedParts)

let hasFrontMatterableExtension path =
    let fmExts = [| "md"; "markdown"; "textile"; "rst"; "htm"; "html"; "txt"; "css"; "js"|] 
    fmExts |> Seq.exists (fun pe -> "." + pe = Path.GetExtension(path)) 
    
let hasPandocableExtension path = 
    let pandocExtensions = [| "md"; "markdown"; "textile"; "rst";|] 
    pandocExtensions |> Seq.exists (fun pe -> "." + pe = Path.GetExtension(path)) 

let getOutPath (inRootPath:string) (outRootPath:string) (path:string) =
    let docRootRelativePath = 
        path.Substring(inRootPath.Length+1(*Hack*)) 
        |> numberWang 
        |> (fun path -> if hasPandocableExtension path then Path.ChangeExtension(path, "html") else path)
    let fullPath = 
        Path.Combine(outRootPath.ToLower(), docRootRelativePath)
    (fullPath.ToLower(), docRootRelativePath.ToLower())

let validFile (filePath:string) = 
    not (filePath.EndsWith("~") || filePath.EndsWith(".swp"))

let buildTocs args = 
    //Do TOC processing for each scope.
    let scopeTocs = 
        args.Scopes
        |> Seq.map (fun scope ->
            let dirs = 
                let path = Path.Combine(args.DocsInPath, scope)
                Directory.GetDirectories(path)
            let getHref path = 
                let (_, relativePath) = getOutPath args.DocsInPath args.DocsOutPath path
                let path = relativePath.Replace(Path.DirectorySeparatorChar, '/')
                "/" + path.Replace(Path.AltDirectorySeparatorChar, '/')
            let toc = 
                [ for dir in dirs do 
                    yield [
                        let filePaths = Directory.GetFiles(dir)
                        for path in filePaths do
                            //TODO this bit of code is duplicated.  Sort.
                            use stream = File.Open(path, FileMode.Open, FileAccess.Read)
                            use reader = new StreamReader(stream)
                            let fmArgs = argsFromFrontMatter reader
                            let title = 
                                let x = getArgValueOpt "toc-title" fmArgs
                                let x = if x.IsSome then x else getArgValueOpt "title" fmArgs
                                if x.IsSome then x.Value else emptyTocTitleText 
                            //Check for TOC flag
                            let incInToc = 
                                let tocArg = getArgValueOpt "toc" fmArgs
                                if tocArg = None then true else Convert.ToBoolean(tocArg.Value)
                            if incInToc then
                                yield {Text=title; Link = getHref path}
                    ]
                ]
            (scope + "-toc",toc)
            )
        |> List.ofSeq
    
    //Create the TOC html fragment.
    [
        for (scope, tocSections) in scopeTocs do
            //Build the scope TOC
            //TODO make this a template (with default hardcoded)
            let sb = StringBuilder()
            sb.Append("<ul class='nav nav-list'>") |> ignore
            for tocSection in tocSections do
                let te = tocSection.Head//We assume head is the seciton header.
                let liFormat = "<li><a href='{0}'>{1}</a></li>"
                sb.AppendFormat(liFormat, te.Link, te.Text) |> ignore
                sb.Append("<ul>") |> ignore
                for te in tocSection.Tail do
                    sb.AppendFormat(liFormat, te.Link, te.Text) |> ignore
                sb.Append("</ul>") |> ignore
            sb.Append("</ul>") |> ignore
            yield (scope, sb.ToString())
    ] 

///A scope is a bunch of pages that have a TOC.
let buildPages (args:ExpandocArgs) = 

    //Generate TOCs for folder scopes.
    let tocArgs = 
        buildTocs args
        |> Seq.map (fun (scope,toc) -> Value(scope, toc))
        |> List.ofSeq

    //Get template parents map
    let templates = 
        Directory.GetFiles(args.TemplatesPath)
        |> Seq.filter validFile
        |> Seq.map (fun templatePath ->
            use stream = File.Open(templatePath, FileMode.Open, FileAccess.Read)
            use reader = new StreamReader(stream)
            let fmArgs = argsFromFrontMatter reader
            let parent = getArgValueOpt "template" fmArgs
            let template = reader.ReadToEnd()
            let templateFile = Path.GetFileName(templatePath)
            let values = valueTuples fmArgs |> List.ofSeq
            (templateFile, {FileName=templateFile; Template=template; ParentFileName=parent; Vars = values})
        )
        |> Map.ofSeq

    //Copy over files and convert the ones that need it.
    let fileProcessingInfos = 
        //Get all the files.
        let excludedDirFilter dir = DirectoryInfo(dir).Name.StartsWith("_")
        seq { yield args.DocsInPath; yield! seqDirsBelow args.DocsInPath excludedDirFilter }
        |> seqFilesIn
        |> Seq.choose (fun path -> 
            //Numberwang the output path.
            let (outPath, relativeOutPath) = getOutPath args.DocsInPath args.DocsOutPath path
            if hasFrontMatterableExtension path then
                //Read and convert (if required) the input.
                use stream = File.Open(path, FileMode.Open, FileAccess.Read)
                use reader = new StreamReader(stream)
                //Get args from front matter.
                let fmArgs = List.append (argsFromFrontMatter reader) tocArgs 
                //Convert if required.
                let (errCode, errMsg, output) = 
                    //Pandoc.toHtml args.PandocPath fmArgs reader
                    if hasPandocableExtension path then
                        Pandoc.toHtml args.PandocPath [] reader
                    else (0,"", reader.ReadToEnd())
                //Get the layout/template path
                let templateName = getArgValueOpt "template" fmArgs
                //Get all the vars in a map for the template
                let vars = valueTuples fmArgs
                let output = 
                    if templateName.IsSome then 
                        let fullLayoutPath = Path.Combine(args.TemplatesPath, templateName.Value)
                        nustache output templateName.Value templates vars 
                    else output
                //Write the output
                if errCode = 0 then writeTextFile output outPath
                else printfn "Error %s for %s." errMsg path
                Some({InPath=path; OutPath=outPath; RelativeOutPath=relativeOutPath;})
            else
                if validFile outPath then
                    ensureDir <| Path.GetDirectoryName(outPath)
                    File.Copy(path, outPath, true)
                    Some({InPath=path; OutPath=outPath; RelativeOutPath=relativeOutPath;})
                else
                    None 
            )
        |> List.ofSeq

    ()

///Run this app yo!
let runApp (args:ExpandocArgs) =

    //Initial clean and build.
    cleanDirectory args.DocsOutPath
    let results = buildPages args

    //Web server output (if requested) and auto-build on changes.
    match args.HttpServerPort with
    | Some(port) -> 
        //Watch for rebuilds...
        let fsw = new FileSystemWatcher()
        fsw.Path <- args.DocsInPath
        let rebuildSite e = buildPages args |> ignore
        fsw.Changed.Add(rebuildSite)
        fsw.Deleted.Add(rebuildSite)
        fsw.Created.Add(rebuildSite)
        fsw.Renamed.Add(rebuildSite)
        fsw.NotifyFilter <- NotifyFilters.LastWrite
        fsw.EnableRaisingEvents <- true
        fsw.IncludeSubdirectories <- true
        //Serve the site up.
        fileSystemServer args.DocsOutPath args.HttpServerPort.Value
        Console.ReadLine() |> ignore
        0//for success!
    | None -> 0 
