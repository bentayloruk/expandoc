module Nustache
open Nustache.Core
open System.Collections.Generic
open Arg

type Template = { FileName:string; Template:string; ParentFileName:string option; Vars: (string*string) list}

let nustache content templateName (templatesMap:Map<string,Template>) vars =
    //Recurse up the parents, rendering the childs render.
    let rec inner content' templateName' vars' =
        match templatesMap.TryFind templateName' with
        | Some(template) -> 
            //Join template args with current args
            let varAcc = 
                seq {yield! vars'; yield! template.Vars } 
                |> Seq.filter (fun (name,_) -> name <> "template")//HACK!!!
                |> List.ofSeq
            //Add to dictionary.
            let dic = Dictionary<string,obj>()
            for (k,v) in varAcc do dic.Add(k, v)
            //TODO this is Expandoc specific and not nustachey.
            dic.Add("content", content')
            let output = Render.StringToString(template.Template, dic)
            match template.ParentFileName with
            | Some(parent) ->  inner output parent varAcc 
            | None -> output
        | None -> printfn "Missing template %s" templateName'; content' //failwith <| sprintf "No template named %s." templateName'
    inner content templateName vars 