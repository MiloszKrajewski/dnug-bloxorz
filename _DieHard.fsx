#load "BFS.fsx"

module DieHard = 
    open Bloxorz

    type Jug = | A | B
    type Level = int
    type State = Map<Jug, Level>
    type World = Map<Jug, Level>
    type Move =
        | None
        | Empty of Jug
        | Fill of Jug
        | Transfer of Jug * Jug

    let init maxA maxB = [ (A, maxA); (B, maxB) ] |> Map.ofSeq

    let move world state move = 
        match move with
        | Empty jug -> state |> Map.add jug 0
        | Fill jug -> 
            let maxJ = world |> Map.find jug
            state |> Map.add jug maxJ
        | Transfer (jugA, jugB) when jugA <> jugB -> 
            let maxB = world |> Map.find jugB
            let levelA = state |> Map.find jugA
            let levelB = state |> Map.find jugB
            let maxAB = min levelA (maxB - levelB)
            state |> Map.add jugA (levelA - maxAB) |> Map.add jugB (levelB + maxAB)
        | _ -> failwithf "Invalid move: %A" move

    let solve world target = 
        let state = init 0 0
        let idof s = sprintf "%d.%d" (s |> Map.find A) (s |> Map.find B)
        let fanout s = 
            [Empty A; Empty B; Fill A; Fill B; Transfer (A, B); Transfer (B, A)]
            |> Seq.map (fun m -> m, move world s m)
        BFS.trace idof fanout state
        |> Seq.tryFind (fun (s, _) -> Map.find A s = target || Map.find B s = target)

let demo () =
    let world = DieHard.init 117 95
    let solution = DieHard.solve world 4
    let moves = solution |> Option.map snd |> Option.defaultValue [] |> List.rev

    moves 
    |> List.scan (fun s m -> DieHard.move world s m) (DieHard.init 0 0) 
    |> List.skip 1
    |> List.zip moves
    |> List.map (fun (m, s) -> sprintf "%A -> (%d, %d)" m (s |> Map.find DieHard.A) (s |> Map.find DieHard.B))

demo () |> List.iter (printfn "%s")