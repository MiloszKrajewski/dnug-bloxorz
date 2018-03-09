#load "BFS.fsx"

open System.IO
open BFS

let scan root =
    let node = DirectoryInfo(root)
    let idof (d: DirectoryInfo) = d.FullName.ToLower()
    let fanout (d: DirectoryInfo) = try d.EnumerateDirectories("*") with | _ -> Seq.empty
    node |> BFS.traverse idof fanout

scan "." |> Seq.map (fun d -> d.FullName) |> Seq.iter (printfn "%s")
