module bloxorz

open Fable.Import
open Fable.Core
open Fable.Core.JsInterop
open System.Collections.Generic

let canvas = Browser.document.getElementById("canvas") :?> Browser.HTMLCanvasElement
let button = Browser.document.getElementById("start") :?> Browser.HTMLButtonElement
let context = canvas.getContext_2d()

let resize () =
    canvas.width <- Browser.window.innerWidth
    canvas.height <- Browser.window.innerWidth

let init() =
    context.fillStyle <- !^"rgb(200,0,0)"
    context.fillRect (10., 10., 55., 50.)
    context.fillStyle <- !^"rgba(0, 0, 200, 0.5)"
    context.fillRect (30., 30., 55., 50.)

let animate paint (states: 'state seq) =
    let enumerator = states.GetEnumerator ()
    let rec action () =
        if enumerator.MoveNext () then
            paint enumerator.Current
            Browser.window.setTimeout (action, 1000) |> ignore
    action ()

let paint number = printfn "%A" number

Browser.window.addEventListener_load (fun _ -> printf "Loaded"; resize (); init (); null)
Browser.window.addEventListener_resize (fun _ -> printf "Resized"; resize (); null)
button.addEventListener_click (fun _ -> printf "Clicked"; [1; 2; 3; 4; 5] |> animate paint; null)
