﻿module HttpServer

open System; open System.IO; open System.Collections.Generic; open System.Globalization
open IO; open Mime
open Firefly; open Owin; open Gate

let private writeLn (s:string) = Console.WriteLine(s)

let fileSystemServer path (port:int) =
    let server = Firefly.Http.ServerFactory()
    let requestHandler (a:IDictionary<string,System.Object>) (resultDelegate:ResultDelegate) (err:Action<Exception>) = 
        //Serve the pages...

        writeLn path 

        //Get file system path (tag index on if a dir request)
        let path = 
            let requestPath = a.[OwinConstants.RequestPath].ToString()
            if requestPath.EndsWith("/") then 
                path + requestPath + "index.html" 
            else path + requestPath
            
        let mimeType = 
            let fileExtension = Path.GetExtension(path).Substring(1)
            mimeTypeForFileExtension fileExtension

        //Create the response
        let response = 
            if File.Exists(path) then
                match mimeType with
                | Some(mt) -> 
                    match isBinaryMimeType mt with
                    | false -> 
                        use sr = new StreamReader(path)
                        (200, Some(sr.ReadToEnd()), None, mt)
                    | true -> 
                        use br = new BinaryReader(File.Open(path, FileMode.Open))
                        let fileInfo = FileInfo(path)
                        (200, None, Some(br.ReadBytes(int fileInfo.Length)), mt)
                | None -> failwith "File extension not mapped to MIME type."
            else (404, None, None, "") 
        
        //Serve the response
        match response with 
        | (200, Some(text), _, mime) -> 
            let response = Response(resultDelegate, ContentType = mime)
            response.Write(text) |> ignore
            response.End()
        | (200, _, Some(data), mime) -> 
            let response = Response(resultDelegate, ContentType = mime)
            response.Headers.["Content-Range"] <- [| sprintf "bytes 0-%i" (data.Length - 1) |];
            response.Headers.["Content-Length"] <- [| data.Length.ToString(CultureInfo.InvariantCulture) |];
            response.Write(new ArraySegment<byte>(data)) |> ignore
            response.End()
        (*|(404, body)*)
        | (404, _,_,_) -> 
            printfn "404 for %s." path
            let response = Response(resultDelegate, ContentType = "")
            response.Write("Not found") |> ignore
            response.End()
        | (resCode, _,_,_) -> failwith "Unhandled response code %i." resCode


        ()
    let server = server.Create(requestHandler, port)
    writeLn ("Running Firefly on port " + port.ToString())
    ()

