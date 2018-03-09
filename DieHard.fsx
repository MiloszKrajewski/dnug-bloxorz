#load "BFS.fsx"

open BFS

type Jug = | A | B
type Level = int
type State = Map<Jug, Level>
type World = Map<Jug, Level>
type Move =
    | Empty of Jug
    | Fill of Jug
    | Transfer of Jug * Jug

let init (a: Level) (b: Level) = [ (A, a); (B, b) ] |> Map.ofSeq

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
    let idof state =
        let find j = state |> Map.find j
        sprintf "%d.%d" (find A) (find B)
    let fanout state =
        [Empty A; Empty B; Fill A; Fill B; Transfer (A, B); Transfer (B, A)]
        |> Seq.map (fun m -> m, move world state m)
    BFS.trace idof fanout state
    |> Seq.tryFind (fun (s, _) -> [A; B] |> Seq.exists (fun j -> s |> Map.find j = target))

let demo () =
    let world = init 113 11
    let solution = solve world 47
    let moves = solution |> Option.map snd |> Option.defaultValue [] |> List.rev
    moves
    |> List.scan (move world) (init 0 0)
    |> List.skip 1
    |> List.zip moves
    |> List.map (fun (m, s) -> sprintf "%A -> (%d, %d)" m (s |> Map.find A) (s |> Map.find B))

demo () |> Seq.iter (printfn "%s")
