module FrontMatter
open System
open System.IO
open System.Text
open System.Web.Script.Serialization
open System.Collections.Generic


type FrontMatter = 
    | Json of (*json*) obj
    | UnknownOrBad of (*The front matter we can't parse*) string
    | NoFrontMatter 

let readFrontMatter (streamReader:StreamReader) =
    if streamReader.ReadLine() = "---" then
        //Looks like we have front matter to read, so read it.
        let sb = StringBuilder()
        let rec readUntilFrontMatterEnds () =
            match streamReader.ReadLine() with
            | "---" -> ()
            | frontMatterData -> 
                sb.AppendLine(frontMatterData) |> ignore
                readUntilFrontMatterEnds ()
        readUntilFrontMatterEnds ()
        let fm = sb.ToString()
        //Try and parse it into a known format.
        try 
            let js = JavaScriptSerializer()
            let json = js.DeserializeObject(fm)
            Json(json)
        with 
           | ex -> printfn "%s" ex.Message; UnknownOrBad(fm)
    else
        //HACK reset the stream.  This should work for us but is rank.
        streamReader.DiscardBufferedData()
        let _ = streamReader.BaseStream.Seek(0L, SeekOrigin.Begin)
        streamReader.BaseStream.Position <- 0L
        NoFrontMatter



(*
//NOTE - Originally used Yaml (like Jekyll) but decided JSON was a better idea.
//Reads "front matter" which is content delimited by "---" (with the first --- being the first line of the stream.
let readFrontMatter(streamReader:StreamReader) =
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
        //HACK reset the stream.  This should work for us but is rank.
        streamReader.DiscardBufferedData()
        let _ = streamReader.BaseStream.Seek(0L, SeekOrigin.Begin)
        streamReader.BaseStream.Position <- 0L
        ""

///Returns a list of (switch, value) tuples for any pandoc args in the yaml.  Empty list when none.
let argsFromText (yaml:string) (templatesPath:string) =
    match yaml with
    | "" -> []
    | _ ->
        //Yield from the Yaml!
        use yr = new StringReader(yaml)
        let ys = YamlStream()
        ys.Load(yr)
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
*)
