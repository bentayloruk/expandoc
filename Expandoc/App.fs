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

type FileInfo = { InPath:string; OutPath:string; RelativeOutPath:string; FrontMatter:string; IncludeInToc:bool}
type TocEntry = { Text:string; Link:string}

//Reads "front matter" which is content delimited by "---" (with the first --- being the first line of the stream.
let readFrontMatter (streamReader:StreamReader) =
    if streamReader.ReadLine() = "---" then
        let sb = StringBuilder()
        sb.AppendLine("---") |> ignore
        let rec readUntilFrontMatterEnds () =
            match streamReader.ReadLine() with
            | "---" -> sb.AppendLine("...") |> ignore//Use 3 dots as that is Yaml for end of Yaml document.  --- starts another one
            | frontMatterData -> 
                sb.AppendLine(frontMatterData) |> ignore
                readUntilFrontMatterEnds ()
        readUntilFrontMatterEnds ()
        sb.ToString()
    else
        ""
///Returns a list of (switch, value) tuples for any pandoc args in the yaml.  Empty list when none.
let argsFromYaml (yaml:string) (templatesPath:string) =
    match yaml with
    | "" -> []
    | _ ->
        //Yield from the Yaml!
        use yr = new StringReader(yaml)
        let ys = new YamlStream()
        ys.Load(yr) |> ignore
        let ymn = ys.Documents.[0].RootNode :?> YamlMappingNode
        [
            for node in ymn.Children do
                match (node.Key.ToString(), node.Value.ToString()) with
                | ("layout", template) -> 
                    let fullPath = Path.Combine([|templatesPath;template;|])
                    if File.Exists(fullPath) then
                        yield Value("layout", fullPath)
                    else 
                        printfn "Missing template %s." fullPath
                | ("title", title) -> 
                    yield! [KeyValue("variable", "pagetitle", title); KeyValue("variable", "title", title)]
                | _ -> ()
        ]

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

let makeTocEntiresForPath fileInfos path =
        [
            //NOTE - This means scopes should not be numberwangy.
            for fileInfo in fileInfos do
                if fileInfo.IncludeInToc && fileInfo.InPath.StartsWith(path) then
                    //Folder bread crumb
                    let breadCrumbs = 
                        fileInfo.RelativeOutPath.Split([|Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar|], StringSplitOptions.RemoveEmptyEntries)
                    //TODO only supporting top level scope and one child folder for now.
                    if breadCrumbs.Length = 3 then
                        //Get the heading...
                        let heading = 
                            let headingFromH1s = headOrDefault "No Title"
                            (loadHtmlFile fileInfo.OutPath).DocumentNode
                            |> getElementTexts "h1" 
                            |> headingFromH1s 
                        printfn "Including %s in Toc with heading %s." fileInfo.InPath heading
                        let siteRootedUrl = "/" + String.Join("/", breadCrumbs)
                        yield (fileInfo, { Link=siteRootedUrl; Text=heading;})
                    else
                        printfn "Skipping TOC for %s." fileInfo.InPath
        ]

let getOutPath (inRootPath:string) (outRootPath:string) (path:string) =
    let docRootRelativePath = 
        path.Substring(inRootPath.Length+1(*Hack*)) 
        |> numberWang 
        |> (fun path -> if hasPandocableExtension path then Path.ChangeExtension(path, "html") else path)
    let fullPath = 
        Path.Combine(outRootPath.ToLower(), docRootRelativePath)
    (fullPath.ToLower(), docRootRelativePath.ToLower())

        
///A scope is a bunch of pages that have a TOC.
let buildPages (args:ExpandocArgs) = 

    let fileProcessingInfos = 
        //Get all the files.
        let excludedDirFilter dir = DirectoryInfo(dir).Name.StartsWith("_")
        seq { yield args.DocsInPath; yield! seqDirsBelow args.DocsInPath excludedDirFilter }
        |> seqFilesIn
        |> Seq.map (fun path -> 
            //Numberwang the output path.
            let (outPath, relativeOutPath) = getOutPath args.DocsInPath args.DocsOutPath path
            if hasFrontMatterableExtension path then
                //Read and convert (if required) the input.
                use stream = File.Open(path, FileMode.Open, FileAccess.Read)
                use reader = new StreamReader(stream)
                //Get args from front matter.
                let yamlFrontMatter = readFrontMatter reader
                let yamlArgs = argsFromYaml yamlFrontMatter args.TemplatesPath
                let includeInToc = yamlFrontMatter.Contains("tocex") <> true//Hack for now... 
                //Convert if required.
                let (errCode, errMsg, output) = 
                    //Pandoc.toHtml args.PandocPath yamlArgs reader
                    if hasPandocableExtension path then
                        Pandoc.toHtml args.PandocPath [] reader
                    else (0,"", reader.ReadToEnd())
                //Get the layout/template path
                let templateName = 
                    let getLayout = function | Value("layout", v) -> Some(v) | _ -> None 
                    yamlArgs |> Seq.tryPick getLayout
                //Get all the vars in a map for the template
                let vars = 
                    let valueChooser = function | Value(n,v) -> Some((n,v)) | _ -> None
                    yamlArgs |> Seq.choose valueChooser
                let output = 
                    if templateName.IsSome then 
                        let fullLayoutPath = Path.Combine(args.TemplatesPath, templateName.Value)
                        let vars = seq { yield ("content", output); yield! vars }
                        nustache fullLayoutPath vars 
                    else output
                //Write the output
                if errCode = 0 then writeTextFile output outPath
                else printfn "Error %s for %s." errMsg path
                {InPath=path; OutPath=outPath; RelativeOutPath=relativeOutPath; FrontMatter=yamlFrontMatter; IncludeInToc=includeInToc}
            else
                ensureDir <| Path.GetDirectoryName(outPath)
                File.Copy(path, outPath, true)
                {InPath=path; OutPath=outPath; RelativeOutPath=relativeOutPath; FrontMatter=""; IncludeInToc=false}
            )
        |> List.ofSeq

    //Do TOC processing for each scope.
    let scopeTocs = 
        args.Scopes
        |> Seq.map (fun scope ->
            let path = Path.Combine(args.DocsInPath, scope)
            let dirs = Directory.GetDirectories(path)
            [ for dir in dirs do yield makeTocEntiresForPath fileProcessingInfos dir ] )
        |> List.ofSeq
    
    //Create the TOC html fragment.
    //TODO the TOC stuff is all a bit ugly.  Fix this up.
    for scopeToc in scopeTocs do
        //Build the scope TOC
        let tocHtml = 
            let sb = StringBuilder()
            sb.Append("<ul class='nav nav-list'>") |> ignore
            for tocSection in scopeToc do
                let (fileInfo, tocEntry) = tocSection.Head//We assume head is the seciton header.
                let liFormat = "<li><a href='{0}'>{1}</a></li>"
                sb.AppendFormat(liFormat, tocEntry.Link, tocEntry.Text) |> ignore
                sb.Append("<ul>") |> ignore
                for (fileInfo, tocEntry) in tocSection.Tail do
                    sb.AppendFormat(liFormat, tocEntry.Link, tocEntry.Text) |> ignore
                    sb.Append("</ul>") |> ignore
            sb.Append("</ul>") |> ignore
            sb.ToString()
        //Write the scope TOC.
        for tocSection in scopeToc do
            for (fileInfo, _) in tocSection do
                let html = readFileCont fileInfo.OutPath (fun reader -> reader.ReadToEnd()) 
                let htmlWithToc = html.Replace("<nav/>", tocHtml)
                writeTextFile htmlWithToc fileInfo.OutPath
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
