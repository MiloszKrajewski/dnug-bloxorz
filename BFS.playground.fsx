#load "BFS.fsx"

open System.IO
open Bloxorz

// https://en.wikipedia.org/wiki/Breadth-first_search
let map =
    [
        1, [2; 3; 4]
        2, [5; 6]
        3, []
        4, [7; 8]
        5, [9; 10]
        6, []
        7, [11; 12]
        8, []
        9, []
        10, []
        11, []
        12, []
    ] |> Map.ofSeq

1 |> BFS.traverse id (fun n -> map |> Map.find n) |> Seq.toList
1 |> BFS.trace id (fun n -> map |> Map.find n |> Seq.map (fun nn -> n, nn)) |> Seq.toList

let scan root =
    DirectoryInfo(root)
    |> BFS.traverse (fun d -> d.FullName.ToLower()) (fun d -> try d.EnumerateDirectories("*") with | _ -> Seq.empty)

scan "." |> Seq.map (fun d -> d.FullName) |> Seq.iter (printfn "%s")
