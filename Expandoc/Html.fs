module Html 

open System.Xml
open System
open System.IO
open System.Text.RegularExpressions
open HtmlAgilityPack 

let parseHtml (html:string) =
    let htmlDoc = HtmlDocument()
    htmlDoc.LoadHtml(html)
    htmlDoc

let loadHtmlFile (path:string) =
    let htmlDoc = HtmlDocument()
    htmlDoc.Load(path)
    htmlDoc

//Returns fun that captures the stream and returns new HtmlDocument each call.
let createDoc (stream:Stream) =
    fun () ->
        stream.Position <- 0L//Hmmm should leave this to client
        let doc = HtmlDocument()
        doc.Load(stream)
        doc

//XPath nodes from the document node.  Returns node list (empty if none).
let selectNodes (node:HtmlNode) xPath = 
    let nc = node.SelectNodes xPath
    match nc with
    | null -> List.empty<HtmlNode>
    | nodes -> nodes |> List.ofSeq

//Slugifies a string from "Hello Ben Page" to "hello-ben-page"
let slugify (s:string) =
    Regex.Replace(s.Trim(), "[? :]" , "-").ToLower()

//Gets the inner texts for all elements of element in the html.
let getElementTexts element node =
    seq 
        { 
        yield! (selectNodes node (sprintf "//%s" element)) 
        |> Seq.map (fun n -> n.InnerText) 
        }
    |> List.ofSeq

    (*
let private selectSplitNodes node = selectNodes node @"//h1"
    

//Save the pages
let saveDoc path name (doc:HtmlDocument) =
    let formatPath (fileName:string) =
        Directory.GetParent(path).ToString() + "\\" + fileName + ".html" 
    let formatPath = formatPath << slugify
    doc.Save(formatPath (name))
    
let rec removeSiblings (node:HtmlNode) (getSibling:(HtmlNode->HtmlNode)) =
    match getSibling node with
    | null -> ()
    | sibling -> 
        removeSiblings sibling getSibling
        if sibling.Name <> "#text" then
            node.ParentNode.RemoveChild(sibling) |> ignore

let removePrevSiblings node =
    removeSiblings node (fun node -> node.PreviousSibling)

let removeNextSiblings node =
    removeSiblings node (fun node -> node.NextSibling)


let makeDocPerSplitElement (docMaker:(unit->HtmlDocument)) =
    //Ugly.  We create one doc just to find the split node count.
    let doc = docMaker() 
    let splitNodes = selectSplitNodes doc.DocumentNode
    //Then we create one for each split node and hack elements out.
    seq {
        for i = 0 to splitNodes.Length - 1  do
            let doc = docMaker()
            let splitNodes = selectSplitNodes doc.DocumentNode
            removePrevSiblings splitNodes.[i]
            let nextSplitSibling = i + 1
            if nextSplitSibling < splitNodes.Length then
                removeNextSiblings splitNodes.[nextSplitSibling].PreviousSibling
            yield (doc, splitNodes.[i].InnerText)
        }

//Removes the heading to TOC links that Pandoc adds.
let removeTocBackLinks (doc:HtmlDocument) =
    selectNodes doc.DocumentNode @"//*[@href='#TOC']"
    |> List.iter(fun link -> 
        let text = HtmlTextNode.CreateNode(link.InnerText)
        let header = link.ParentNode
        header.RemoveAllChildren()
        header.ChildNodes.Add(text))
    doc

//Updates the TOC hrefs to point to the split pages
let updateLinks (docs:list<HtmlDocument*string>) =
    for (doc,_) in docs do
        for liIndex = 1 to docs.Length do
            let slug = slugify (snd docs.[liIndex-1]) + ".html"
            selectNodes doc.DocumentNode (sprintf "//*[@id='TOC']/ul/li[%s]//a" <| liIndex.ToString())
            |> List.iter(fun link -> 
                let href = (link.Attributes.["href"]).Value
                if not (href.StartsWith(slug)) then
                    let a = slug + href
                    link.Attributes.["href"].Value <- a
                )

//Add the nav and nav-list to classes.
let addStyles (doc:HtmlDocument) =
    let tocUl =
        selectNodes doc.DocumentNode @"//*[@id='TOC']/ul" 
        |> Seq.head
    if not (tocUl.Attributes.Contains("class")) then tocUl.Attributes.Add("class", "")
    tocUl.Attributes.["class"].Value <- "nav nav-list"
    doc

let expandocIt (docFactory:Unit->HtmlDocument) =
    //Assume Pandoc has all H1s as siblings.
    let doc = docFactory() 
    let docs = 
        makeDocPerSplitElement docFactory
        |> Seq.map (fun (doc,text) -> (removeTocBackLinks doc, text))
        |> Seq.map (fun (doc,text) -> (addStyles doc, text))
        |> List.ofSeq
    updateLinks docs
    docs
                                
*)





