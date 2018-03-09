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

