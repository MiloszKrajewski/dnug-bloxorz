#load "bfs.fsx"

namespace Bloxorz

module Domain =
    type Position = int * int
    type Bloxor = Position * Position
    type Move = | North | South | East | West
    type Path = Bloxor * Move list
    type World = {
        A: Position
        B: Position
        IsValid: Position -> bool
    }

module World =
    open Domain
    let infinite a b = { A = a; B = b; IsValid = fun _ -> true }

    let parse lines =
        let map = lines |> Seq.mapi (fun y l -> l |> Seq.mapi (fun x c -> (x, y), c)) |> Seq.collect id |> Map.ofSeq
        let a = map |> Map.findKey (fun _ c -> c = 'A')
        let b = map |> Map.findKey (fun _ c -> c = 'B')
        let valid k = map |> Map.tryFind k |> Option.filter (fun c -> c <> ' ') |> Option.isSome
        { A = a; B = b; IsValid = valid }

module Bloxor =
    open Domain

    let shift da db bloxor =
        let shift1 (dx, dy) (x, y) = (x + dx, y + dy)
        let a, b = bloxor
        shift1 da a, shift1 db b

    let (|IsStanding|IsHorizontal|IsVertical|) bloxor =
        let ((ax, ay), (bx, by)) = bloxor
        match bx - ax, by - ay with
        | 0, 0 -> IsStanding
        | 1, 0 -> IsHorizontal
        | 0, 1 -> IsVertical
        | _ -> failwithf "Invalid bloxor: (%d,%d),(%d,%d)" ax ay bx by

    let make position = (position, position)

    let move bloxor direction =
        let shiftX x1 x2 = shift (x1, 0) (x2, 0)
        let shiftY y1 y2 = shift (0, y1) (0, y2)
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

module Solver =
    open Domain

    let solve world =
        let isValid bloxor = let (a, b) = bloxor in world.IsValid a && world.IsValid b
        let validMoves bloxor =
            [North; East; South; West]
            |> Seq.map (fun d -> d, Bloxor.move bloxor d)
            |> Seq.filter (fun (_, b) -> isValid b)
        let start = Bloxor.make world.A
        let goal = Bloxor.make world.B
        let idof ((ax, ay), (bx, by)) = sprintf "%d.%d.%d.%d" ax ay bx by
        BFS.trace idof validMoves start |> Seq.tryFind (fun (bloxor, _) -> bloxor = goal)
