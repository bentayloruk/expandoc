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
                    let fullPath = Path.Combine([|templatesPath;template + ".html";|])
                    yield Value("template", fullPath)
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

let hasPandocableExtension path = 
    let pandocExtensions = [| "md"; "markdown"; "textile"; "rst";|] 
    pandocExtensions |> Seq.exists (fun pe -> "." + pe = Path.GetExtension(path)) 

///A scope is a bunch of pages that have a TOC.
let buildPages (args:ExpandocArgs) = 

    let getOutPath (path:string) =
        let docRootRelativePath = 
            path.Substring(args.DocsInPath.Length+1(*Hack*)) 
            |> numberWang 
        Path.Combine(args.DocsOutPath, docRootRelativePath)
        |> (fun path -> if hasPandocableExtension path then Path.ChangeExtension(path, "html") else path)
        |> (fun path -> path.ToLower()) 

    //Get the first H1 as out TOC entry
    (*
    let createTocEntry  = 
        let pagePath = "/" + scopeDir + "/" + Directory.GetParent(outPath).Name + "/" + Path.GetFileName(outPath)
        seq { yield! getElementTexts "h1" html } 
        |> List.ofSeq |> function 
        | [] -> { TocEntry.Text = "??"; Url = pagePath; Visible = isVisible} 
        | h::_ -> { TocEntry.Text = h; Url = pagePath; Visible = isVisible}


    //Build the TOC html 
    let tocHtml = 
        let sb = StringBuilder()
        sb.Append("<ul class='nav nav-list'>") |> ignore
        for (tocEntries, _) in results do
            let tocEntry = tocEntries.Head//We assume head is the seciton header.
            let liFormat = "<li><a href='{0}'>{1}</a></li>"
            sb.AppendFormat(liFormat, tocEntry.Url, tocEntry.Text) |> ignore
            sb.Append("<ul>") |> ignore
            for tocEntry in tocEntries.Tail do
                if tocEntry.Visible then
                    sb.AppendFormat(liFormat, tocEntry.Url, tocEntry.Text) |> ignore
            sb.Append("</ul>") |> ignore
        sb.Append("</ul>") |> ignore
        sb.ToString()

    //Write the TOC to each HTML file, except any that failed
    let replaceNavElementWithToc result =
        let html = 
            let html = readFileCont result.OutPath (fun reader -> reader.ReadToEnd()) 
            html.Replace("<nav/>", tocHtml)
        writeTextFile html result.OutPath

    for (_, conversionResults) in results do
        for result in conversionResults do
            if result.ErrCode = 0 then replaceNavElementWithToc result
            else printfn "Err code %i %s converting file %s." result.ErrCode result.ErrText result.InPath
    *)

    let inOutTocs = 
        //Get all the files.
        let excludedDirFilter dir = DirectoryInfo(dir).Name.StartsWith("_")
        seq { yield args.DocsInPath; yield! seqDirsBelow args.DocsInPath excludedDirFilter }
        |> seqFilesIn
        |> Seq.map (fun path -> 
            //Numberwang the output path.
            let outPath = getOutPath path
            if hasPandocableExtension path then
                //Read and convert (if required) the input.
                use stream = File.Open(path, FileMode.Open, FileAccess.Read)
                use reader = new StreamReader(stream)
                //Get args from front matter.
                let yamlFrontMatter = readFrontMatter reader
                let pandocArgs = argsFromYaml yamlFrontMatter args.TemplatesPath
                let includeInToc = yamlFrontMatter.Contains("tocex") <> true//Hack for now... 
                //Convert if required.
                let (errCode, errMsg, output) = Pandoc.toHtml args.PandocPath pandocArgs reader
                //Write the output
                if errCode = 0 then writeTextFile output outPath
                else printfn "Error %s for %s." errMsg path
                (path, outPath, yamlFrontMatter, includeInToc)
            else
                ensureDir <| Path.GetDirectoryName(outPath)
                File.Copy(path, outPath, true)
                (path, outPath, "", false)
            )
        |> List.ofSeq

    //Do TOC processing for each scope.
    let tocEntries = 
        [
            for scope in args.Scopes do
                //NOTE - This means scopes should not be numberwangy.
                let scopeRoot = Path.Combine(args.DocsOutPath, scope).ToLower()
                for (inPath, outPath, frontMatter, includeInToc) in inOutTocs do
                    if includeInToc && outPath.StartsWith(scopeRoot) then
                        //Folder bread crumb
                        let relativePath = outPath.Substring(scopeRoot.Length)
                        let breadCrumb = relativePath.Split([|Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar|], StringSplitOptions.RemoveEmptyEntries)
                        if breadCrumb.Length = 2 then
                            //Get the heading...
                            let heading = 
                                let headingFromH1s = headOrDefault "No Title"
                                (loadHtmlFile outPath).DocumentNode
                                |> getElementTexts "h1" 
                                |> headingFromH1s 
                            printfn "Including %s in Toc with heading %s." inPath heading
                            yield (scope.ToLower() :: (breadCrumb |> List.ofArray), heading)
                        else
                            printfn "Skipping TOC for %s." inPath
        ]
    
    //Create the TOC html fragment.
    let tocHtml = 
        let sb = StringBuilder()
        sb.Append("<ul class='nav nav-list'>") |> ignore
        for (breadCrumb, title) in tocEntries do
            let tocEntry = tocEntries.Head//We assume head is the seciton header.
            let liFormat = "<li><a href='{0}'>{1}</a></li>"
            let url = "/" + String.Join("/", breadCrumb)
            sb.AppendFormat(liFormat, url, title) |> ignore
            sb.Append("<ul>") |> ignore
            sb.Append("</ul>") |> ignore
        sb.Append("</ul>") |> ignore
        sb.ToString()
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
