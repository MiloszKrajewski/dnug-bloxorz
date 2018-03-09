namespace System.Collections.Generic

type Queue<'T>(items: 'T seq) = 
    let items = ResizeArray(items)
    new() = Queue(Seq.empty)
    member x.Enqueue item = items.Add(item)
    member x.Dequeue () = let result = items.[0] in items.RemoveAt(0); result
    member x.Count = items.Count
