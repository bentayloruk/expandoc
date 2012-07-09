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
open Mime
open Exception

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
type TocEntry = { Text:string; Link:string; HyperLink: bool}

//Change this function to change the template engine.
let template output templateName templates vars =
    nustache output templateName templates vars 

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

let hasFrontMatterableExtension =
    let fmExts = Set.ofList [ ".md"; ".markdown"; ".textile"; ".rst"; ".htm"; ".html"; ".txt"; ".css"; ".js"] 
    fun path -> fmExts.Contains (Path.GetExtension(path))
    
let hasPandocableExtension = 
    let pandocExtensions = Set.ofList [ ".md"; ".markdown"; ".textile"; ".rst"; ]
    fun path -> pandocExtensions.Contains (Path.GetExtension(path))

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

let getFiles dir = 
    let files = Directory.GetFiles(dir)
    [| for file in files do if validFile file then yield file |]

let buildTocs args = 
    //Do TOC processing for each scope.
    let scopeTocs = 
        args.Scopes
        |> Seq.map (fun scope ->
            //Get the top level dirs in scope.
            let dirs = 
                let path = Path.Combine(args.DocsInPath, scope)
                Directory.GetDirectories(path)
            //Get the relative root path.
            let getHref path = 
                let (_, relativePath) = getOutPath args.DocsInPath args.DocsOutPath path
                let path = relativePath.Replace(Path.DirectorySeparatorChar, '/')
                "/" + path.Replace(Path.AltDirectorySeparatorChar, '/')
            let toc = 
                [ for dir in dirs do 
                    //Get files, except those that are binary mime types.
                    let filePaths = 
                        getFiles dir 
                        |> Seq.filter (fun path ->
                            match mimeTypeForPath path with | None -> false | Some(mt) -> not(isBinaryMimeType mt)
                        )
                        |> List.ofSeq 
                    //Only yield a toc section if we have any files!
                    if filePaths.Length > 0 then
                        yield [
                            for path in filePaths do
                                //Do IO and swallow fails.  We will be eventually consistent (maybe :)!
                                //TODO Only swallow the fails if in local server mode.
                                let readArgs p = 
                                    use stream = File.Open(p, FileMode.Open, FileAccess.Read)
                                    use reader = new StreamReader(stream)
                                    argsFromFrontMatter reader
                                let (ioSuccess, fmArgs) = (protect "TOC IO Problem" [] readArgs) path
                                //If IO was success add to TOC.
                                if ioSuccess && argNotPresentOrSetTo "published" fmArgs "true" then
                                    let tocTitle = 
                                        seq { 
                                            yield (getArgValueOpt "toc-title" fmArgs);
                                            yield (getArgValueOpt "title" fmArgs);
                                            yield Some(emptyTocTitleText);
                                            }
                                        |> Seq.pick (fun s -> s)
                                    //Check for TOC flag
                                    let incInToc = 
                                        let tocArg = getArgValueOpt "toc" fmArgs
                                        if tocArg = None then true else Convert.ToBoolean(tocArg.Value)
                                    let hyperlink = 
                                        let tocArg = getArgValueOpt "toc-link" fmArgs
                                        if tocArg = None then true else Convert.ToBoolean(tocArg.Value)
                                    if incInToc then
                                        yield {Text=tocTitle; Link = getHref path; HyperLink = hyperlink}
                    ]
                ]
            let templateScopeName = 
                scope.Replace('\\','-') + "-toc"
                |> (fun s -> s.Replace('/','-')) //Just in case mono.  This code sucks!
            (templateScopeName,toc)
            )
        |> List.ofSeq
    
    //Create the TOC html fragment.
    [
        let formatTocEntry tocEntry = 
            let liContent = 
                if tocEntry.HyperLink then 
                    String.Format("<a href='{0}'>{1}</a>", tocEntry.Link, tocEntry.Text)
                else tocEntry.Text
            String.Format("<li>{0}</li>", liContent)

        for (scope, tocSections) in scopeTocs do
            //Build the scope TOC
            //TODO make this a template (with default hardcoded)
            let sb = StringBuilder()
            sb.AppendFormat("<ul class='nav nav-list toc {0}'>", scope) |> ignore
            for tocSection in tocSections do
                if tocSection.Length > 1 then
                    let te = tocSection.Head//We assume head is the seciton header.
                    sb.Append(formatTocEntry te) |> ignore
                    sb.Append("<ul>") |> ignore
                    for te in tocSection.Tail do
                        sb.Append(formatTocEntry te) |> ignore
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
        getFiles args.TemplatesPath
        |> Seq.filter validFile
        |> Seq.map (fun templatePath ->
            try
                use stream = File.Open(templatePath, FileMode.Open, FileAccess.Read)
                use reader = new StreamReader(stream)
                let fmArgs = argsFromFrontMatter reader
                let parent = getArgValueOpt "template" fmArgs
                let template = reader.ReadToEnd()
                let templateFile = Path.GetFileName(templatePath)
                let values = valueTuples fmArgs |> List.ofSeq
                Some((templateFile, {FileName=templateFile; Template=template; ParentFileName=parent; Vars = values}))
            with
            | :? IOException -> None 
        )
        |> Seq.choose (fun x -> x)
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
                        template output templateName.Value templates vars 
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

    let buildSite () = 
        //Initial clean and build.
        cleanDirectory args.DocsOutPath
        buildPages args

    //Call it here to start, and then below on file detection changes.
    buildSite ()

    //Web server output (if requested) and auto-build on changes.
    match args.HttpServerPort with
    | Some(port) -> 
        //Watch for rebuilds...
        let fsw = new FileSystemWatcher()
        fsw.Path <- args.DocsInPath
        let rebuildSite e = buildSite () |> ignore
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
