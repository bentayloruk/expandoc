module Nustache
open Nustache.Core
open System.Collections.Generic
open Arg

type Template = { FileName:string; Template:string; ParentFileName:string option; Vars: (string*string) list}

let nustache content templateName (templatesMap:Map<string,Template>) vars =

    let toDictionary pairs =
        let dic = Dictionary<string,obj>()
        for (k,v) in pairs do dic.Add(k, v)
        dic

    //Render the leaf document.
    let d = toDictionary vars
    let initialRender = Render.StringToString(content, d)

    //Recurse up the parent templates providing the "content".
    let rec inner content' templateName' vars' =
        match templatesMap.TryFind templateName' with
        | Some(template) -> 
            //Join template args with current args
            let varAcc = 
                seq {yield! vars'; yield! template.Vars } 
                //HACK!!! take out "template" vars as we will get dupes.
                |> Seq.filter (fun (name,_) -> name <> "template")
                |> List.ofSeq
            let dic = toDictionary varAcc
            dic.Add("content", content')
            let output = Render.StringToString(template.Template, dic)
            match template.ParentFileName with
            | Some(parent) ->  inner output parent varAcc 
            | None -> output
        | None -> printfn "Missing template %s" templateName'; content' //failwith <| sprintf "No template named %s." templateName'
    inner initialRender templateName vars 