module gameoflife

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser

type cellState =
    | Alive
    | Dead

let randomGen = System.Random()

let emptyBoard arrayDimension =
    ArrayF.init arrayDimension arrayDimension (fun _ _ -> Dead)

let randomBoard arrayDimension =
    ArrayF.init arrayDimension arrayDimension (fun _ _ -> 
        match randomGen.Next 5 with
        | 0  -> Alive
        | _ -> Dead )

let neighbourArray = Array.init 8 (fun _ -> (0, 0))
let targetArray = Array.init 8 (fun _ -> Dead)

/// Count the amount of alive neighbours of a cell in the given location
let countAliveNeighboursOf arrayDimension x y (state: cellState[][]) =
    match (x, y) with
    | (0, _) | (_, 0) -> 0
    | (x, y) when x = arrayDimension || y = arrayDimension -> 0
    | _ -> 
       Array.set neighbourArray 0 (x-1, y-1)
       Array.set neighbourArray 1 (x-1, y)
       Array.set neighbourArray 2 (x-1, y+1)
       Array.set neighbourArray 3 (x, y-1)          
       Array.set neighbourArray 4 (x, y+1)
       Array.set neighbourArray 5 (x+1, y-1)
       Array.set neighbourArray 6 (x+1, y) 
       Array.set neighbourArray 7 (x+1, y+1)

       neighbourArray
       |> Array.iteri (fun i (x,y) -> targetArray.[i] <- state.[x].[y])

       targetArray
       |> Array.sumBy (fun e ->
            match e with
            | Alive -> 1
            | Dead -> 0
        )

/// Compute the next state of a cell in a location
let cellNextState arrayDimension x y (state: cellState[][]) =
    let cellState = state.[x].[y]
    match cellState with
    | Alive ->
        match countAliveNeighboursOf arrayDimension x y state with
        | 0 | 1 -> Dead
        | 2 | 3 -> Alive
        | _ -> Dead
    | Dead ->
        match countAliveNeighboursOf arrayDimension x y state with
        | 3 -> Alive
        | _ -> Dead

type appStatus =
    | RunningWithDimensions of int
    | Stopped

let mutable app = Stopped
let runApp () =
    let arrayDimension = 
        match app with
        | RunningWithDimensions dimension -> dimension
        | _ -> 8
    let pixelMultiplier = 1024. / (float arrayDimension)

    let canvas = document.getElementsByTagName_canvas().[0]
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^"rgb(0, 0, 0)"

    let mutable state = randomBoard arrayDimension
    let mutable nextState = emptyBoard arrayDimension

    let draw state (ctx: Browser.CanvasRenderingContext2D) =
        ctx.clearRect(0., 0., 1024., 1024.)
        ArrayF.iteri (fun x y state ->
                match state with
                | Alive -> ctx.fillRect (float x * pixelMultiplier, float y * pixelMultiplier, pixelMultiplier, pixelMultiplier)
                | Dead -> ()   
            ) state

    let updateState () =
        ArrayF.setAll Dead nextState
        ArrayF.mapiTo (fun x y _ ->
            cellNextState arrayDimension x y state
        ) state nextState

        let currentState = state
        state <- nextState
        nextState <- currentState    

    let mutable lastDrawn = 0.

    let rec animationCallback (dt: float) =
        if (dt - lastDrawn) > 33.3 then
            lastDrawn <- dt
            updateState ()
            draw state ctx

        match app with
        | RunningWithDimensions _ -> window.requestAnimationFrame animationCallback |> ignore
        | Stopped -> ()

    window.requestAnimationFrame animationCallback |> ignore

let getDimensionSetting () =
        let dimensionInput : HTMLInputElement = !!document.getElementById "arrayDimension"
        int dimensionInput.value

let setUpEvents () =
    let startButton : HTMLButtonElement = !!document.getElementById "startButton"
    startButton.onclick <- fun _ ->
        app <- getDimensionSetting () |> RunningWithDimensions
        runApp ()
        startButton.disabled <- true

    let stopButton : HTMLButtonElement = !!document.getElementById "stopButton"
    stopButton.onclick <- fun _ -> 
        app <- Stopped
        startButton.disabled <- false

setUpEvents ()