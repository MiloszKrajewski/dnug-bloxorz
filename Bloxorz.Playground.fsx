#load "BFS.fsx"
#load "Bloxorz.fsx"

open Bloxorz

let world =
    """
        xxxx
        xBxx
        Axx
    """.Split([|'\n'|])
world |> World.parse |> Solver.solve

// let world = [
//     "      xxxxxxx"
//     "xxxx  xxx  xx"
//     "xxxxxxxxx  xxxx"
//     "xAxx       xxBx"
//     "xxxx       xxxx"
//     "            xxx"
// ]
// world |> World.parse |> Solver.solve

