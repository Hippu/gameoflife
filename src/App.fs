module gameoflife

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser

type cellState =
    | Alive
    | Dead

let arrayDimension = 512
let pixelMultiplier = 1024. / (float arrayDimension)

let randomGen = System.Random()

let emptyBoard =
    ArrayF.init arrayDimension arrayDimension (fun _ _ -> Dead)

let randomBoard =
    ArrayF.init arrayDimension arrayDimension (fun _ _ -> 
        match randomGen.Next 3 with
        | 0  -> Alive
        | _ -> Dead )

let neighbourArray = Array.init 8 (fun _ -> (0, 0))

let countAliveNeighboursOf x y (state: cellState[][]) =
    match (x, y) with
    | (0, _) -> 0
    | (_, 0) -> 0
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
       |> Array.map (fun (x,y) -> state.[x].[y])
       |> Array.sumBy (function | Alive -> 1 | Dead -> 0)

let cellNextState x y (state: cellState[][]) =
    let cellState = state.[x].[y]
    match cellState with
    | Alive ->
        match countAliveNeighboursOf x y state with
        | 0 | 1 -> Dead
        | 2 | 3 -> Alive
        | _ -> Dead
    | Dead ->
        match countAliveNeighboursOf x y state with
        | 3 -> Alive
        | _ -> Dead

let mutable state = randomBoard
let mutable nextState = emptyBoard
let mutable ctx = 
    let canvas = document.getElementsByTagName_canvas().[0]
    canvas.getContext_2d()

let updateState () =
    ArrayF.setAll Dead nextState
    ArrayF.mapiTo (fun x y _ ->
        cellNextState x y state
    ) state nextState

    let currentState = state
    state <- nextState
    nextState <- currentState
    

let draw state (ctx: Browser.CanvasRenderingContext2D) =
    ctx.clearRect(0., 0., 1024., 1024.)
    ArrayF.iteri (fun x y state ->
            match state with
            | Alive -> ctx.fillRect(float x * pixelMultiplier, float y * pixelMultiplier, pixelMultiplier, pixelMultiplier)
            | Dead -> ()   
        ) state

let rec animationCallback (dt: float) =
    match dt with
    | dt when dt > 50. ->
        updateState()
        draw state ctx
    | _ -> ()
    window.requestAnimationFrame animationCallback |> ignore
    

let init() =
    let canvas = document.getElementsByTagName_canvas().[0]
    canvas.width <- 1024.
    canvas.height <- 1024.
    // The (!^) operator checks and casts a value to an Erased Union type
    // See http://fable.io/docs/interacting.html#Erase-attribute
    ctx.fillStyle <- !^"rgb(0, 0, 0)"
    ArrayF.set state 1 1 Alive
    Browser.window.requestAnimationFrame animationCallback |> ignore

init()