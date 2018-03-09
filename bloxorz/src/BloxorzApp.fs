namespace Bloxorz

open Fable.Import
open Fable.Core.JsInterop
open Domain
open Fable

module Main =
    let window = Browser.window
    let element name = Browser.document.getElementById(name)
    let canvas = element("canvas") :?> Browser.HTMLCanvasElement
    let button = element("start") :?> Browser.HTMLButtonElement
    let context = canvas.getContext_2d()

    let resize () =
        canvas.width <- window.innerWidth
        canvas.height <- window.innerHeight

    let drawBloxor (x, y) (color: string) =
        context.fillStyle <- !^ color
        context.fillRect (float (x * 22 + 8), float (y * 22 + 8), 21., 21.)

    let drawWorld (world: World) =
        for x = 0 to 100 do
            for y = 0 to 100 do
                let p = (x, y)
                if world.IsValid p then
                    let color = if p = world.A then "green" else if p = world.B then "red" else "grey"
                    drawBloxor (x, y) color

    let animate paint (states: 'state seq) =
        let enumerator = states.GetEnumerator ()
        let rec action () =
            if enumerator.MoveNext () then
                paint enumerator.Current
                window.setTimeout (action, 1000) |> ignore
        action ()

    let paint world bloxor =
        printfn "%A" bloxor
        drawWorld world
        let a, b = bloxor
        drawBloxor a "blue"
        drawBloxor b "blue"

    let world =
        [
            "      xxxxxxx"
            "xxxx  xxx  xx"
            "xxxxxxxxx  xxxx"
            "xAxx       xxBx"
            "xxxx       xxxx"
            "            xxx"
        ] |> Domain.parse

    let solution =
        world
        |> Solver.solve7
        |> Option.map snd
        |> Option.defaultValue []
        |> List.rev
        |> List.scan (Bloxor.move) (Bloxor.make world.A)

    window.addEventListener_load (fun _ -> printf "Loaded"; resize (); drawWorld world; null)
    window.addEventListener_resize (fun _ -> printf "Resized"; resize (); drawWorld world; null)
    button.addEventListener_click (fun _ -> printf "Clicked"; solution |> animate (paint world); null)
