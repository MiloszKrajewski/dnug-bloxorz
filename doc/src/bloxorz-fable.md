Bloxorz
---
```fsharp
let generic test mark fanout node = 
    let queue = Queue()

    let empty () = queue.Count = 0
    let pop () = queue.Dequeue ()
    let push n = queue.Enqueue n

    node |> push

    seq {
        while not (empty ()) do
            let n = pop ()
            yield n
            mark n
            n |> fanout |> Seq.filter test |> Seq.iter push
    }
```

```fsharp
let generic' test mark fanout node = 
    let queue = Queue([node])
    let rec popAll () = seq { if queue.Count = 0 then yield queue.Dequeue (); yield! popAll () }
    let push n = queue.Enqueue n
    seq { yield! popAll () |> Seq.tap (tap mark >> fanout >> Seq.filter test >> Seq.iter push) }
```

```fsharp
let traverse idof fanout node =
    let visited = HashSet()
    let test n = visited.Contains (idof n) |> not
    let mark n = visited.Add (idof n) |> ignore
    generic test mark fanout node

let trace idof fanout node =
    let idof' node = let state, _ = node in idof state
    let fanout' node = 
      let state, history = node
      state |> fanout |> Seq.map (fun (a, n) -> (n, a :: history))
    let node' = node, []
    traverse idof' fanout' node'
```