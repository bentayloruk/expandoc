module Nustache
open Nustache.Core
open System.Collections.Generic

let nustache templatePath vars =
    let dic = Dictionary<string,obj>()
    for (k,v) in vars do dic.Add(k, v)
    Render.FileToString(templatePath, dic)

