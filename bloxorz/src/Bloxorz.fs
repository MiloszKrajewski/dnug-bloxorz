namespace Bloxorz

module BFS =
    open System.Collections.Generic

    let generic test mark fanout (node: 'node) =
        let queue = Queue()
        queue.Enqueue(node)

        seq {
            while queue.Count <> 0 do
                let curr = queue.Dequeue ()
                yield curr
                mark curr
                curr |> fanout |> Seq.filter test |> Seq.iter (queue.Enqueue)
        }

    let traverse idof fanout node =
        let visited = HashSet()
        let test node = not (visited.Contains (idof node))
        let mark node = visited.Add (idof node) |> ignore
        generic test mark fanout node

    let trace idof fanout node =
        let idof' (state, _) = idof state
        let fanout' node =
            let state, history = node
            state |> fanout |> Seq.map (fun (action, newstate) -> (newstate, action :: history))
        let node' = node, []
        traverse idof' fanout' node'

module Domain =
    type Position = int * int
    type Bloxor = Position * Position
    type Move = | North | East | South | West
    type Path = Bloxor * Move list
    type World = {
        A: Position
        B: Position
        IsValid: Position -> bool
    }

    let infite A B = { A = A; B = B; IsValid = fun _ -> true }

    let parse lines =
        let map =
            lines
            |> Seq.mapi (fun y l -> l |> Seq.mapi (fun x c -> (x, y), c))
            |> Seq.collect id
            |> Map.ofSeq
        let a = map |> Map.findKey (fun k c -> c = 'A')
        let b = map |> Map.findKey (fun k c -> c = 'B')
        let valid k = map |> Map.tryFind k |> Option.filter (fun c -> c <> ' ') |> Option.isSome
        { A = a; B = b; IsValid = valid }

module Bloxor =
    open Domain
    let (|IsStanding|IsHorizontal|IsVertical|) bloxor =
        let ((ax, ay), (bx, by)) = bloxor
        match bx - ax, by - ay with
        | 0, 0 -> IsStanding
        | 1, 0 -> IsHorizontal
        | 0, 1 -> IsVertical
        | _ -> failwithf "Invalid bloxor (%d,%d) (%d,%d)" ax ay bx by

    let move bloxor direction =
        let shiftX x1 x2 ((ax, ay), (bx, by)) = (ax + x1, ay), (bx + x2, by)
        let shiftY y1 y2 ((ax, ay), (bx, by)) = (ax, ay + y1), (bx, by + y2)
        match bloxor, direction with
        | IsStanding, North -> bloxor |> shiftY -2 -1
        | IsStanding, East -> bloxor |> shiftX 1 2
        | IsStanding, South -> bloxor |> shiftY 1 2
        | IsStanding, West -> bloxor |> shiftX -2 -1
        | IsHorizontal, North -> bloxor |> shiftY -1 -1
        | IsHorizontal, East -> bloxor |> shiftX 2 1
        | IsHorizontal, South -> bloxor |> shiftY 1 1
        | IsHorizontal, West -> bloxor |> shiftX -1 -2
        | IsVertical, North -> bloxor |> shiftY -1 -2
        | IsVertical, East -> bloxor |> shiftX 1 1
        | IsVertical, South -> bloxor |> shiftY 2 1
        | IsVertical, West -> bloxor |> shiftX -1 -1

    let make (position: Position): Bloxor = position, position

module Solver =
    open Domain
    open Bloxor
    open BFS
    let solve7 world =
        let isValid (a, b) = world.IsValid a && world.IsValid b
        let validMoves bloxor =
            [North; South; East; West]
            |> Seq.map (fun direction -> direction, move bloxor direction)
            |> Seq.filter (fun (_, newbloxor) -> isValid newbloxor)
        let start = make world.A
        let goal = make world.B
        let idof ((ax, ay), (bx, by)) = sprintf "%d.%d.%d.%d" ax ay bx by
        BFS.trace idof validMoves start
        |> Seq.tryFind (fun (bloxor, history) -> bloxor = goal)
