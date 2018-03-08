namespace Bloxorz

open System.Collections.Generic

module BFS =
    let traverse idof fanout node =
        let visited = HashSet()
        let queue = Queue()

        let empty () = queue.Count = 0
        let pop () = queue.Dequeue ()
        let push n = queue.Enqueue n
        let test n = visited.Contains (idof n) |> not
        let mark n = visited.Add (idof n) |> ignore

        node |> Seq.singleton |> queue.Enqueue
        seq {
            while not (empty ()) do
                for n in pop () |> Seq.filter test do
                    yield n
                    mark n
                    n |> fanout |> push
        }

    let trace idof fanout node =
        let idof' node = let state, _ = node in idof state
        let fanout' node = let state, history = node in state |> fanout |> Seq.map (fun (a, n) -> (n, a :: history))
        let node' = node, []
        traverse idof' fanout' node'
