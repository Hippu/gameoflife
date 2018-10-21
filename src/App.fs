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

/// Scratch array for countAliveNeighbours of function
let neighbourArray = Array.init 8 (fun _ -> (0, 0))
/// Scratch array for countAliveNeighbours of function
let targetArray = Array.init 8 (fun _ -> Dead)

/// Count the amount of alive neighbours of a cell in the given location
/// Uses the neighbourArray and targetArray arrays as scratch memory.
/// This is to optimize the program by minimizing memory allocations
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

/// The app can be in two states:
/// Running with i * i dimensions or not running
type appStatus =
    | RunningWithDimensions of int
    | Stopped

/// The initial state of the app
let mutable app = Stopped

/// The function that starts running the app
let runApp () =
    // Get the dimensions for the "board" that the app is running in
    let arrayDimension = 
        match app with
        | RunningWithDimensions dimension -> dimension
        | _ -> 8
    let pixelMultiplier = 1024. / (float arrayDimension)

    let canvas = document.getElementsByTagName_canvas().[0]
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^"rgb(0, 0, 0)"

    // The application puts it states in these two two-dimensional arrays and swaps between them
    let mutable state = randomBoard arrayDimension
    let mutable nextState = emptyBoard arrayDimension

    /// Draw the current state of the application to the rendering context of the canvas
    let draw state (ctx: Browser.CanvasRenderingContext2D) =
        ctx.clearRect(0., 0., 1024., 1024.)
        ArrayF.iteri (fun x y state ->
                match state with
                | Alive -> ctx.fillRect (float x * pixelMultiplier, float y * pixelMultiplier, pixelMultiplier, pixelMultiplier)
                | Dead -> ()   
            ) state

    /// Update the state of the program according to the rules of 'Game of Life'
    let updateState () =
        ArrayF.setAll Dead nextState
        ArrayF.mapiTo (fun x y _ ->
            cellNextState arrayDimension x y state
        ) state nextState

        let currentState = state
        state <- nextState
        nextState <- currentState    

    /// Timer for when the state the app was last updated
    let mutable lastUpdate = 0.

    /// The callback for updating and drawing the state of the app
    let rec updateAndDraw (dt: float) =
        if (dt - lastUpdate) > 33.3 then
            lastUpdate <- dt
            updateState ()
            draw state ctx

        match app with
        | RunningWithDimensions _ -> window.requestAnimationFrame updateAndDraw |> ignore
        | Stopped -> ()

    window.requestAnimationFrame updateAndDraw |> ignore

/// Read the user inputted size for the board
let getDimensionSetting () =
        let dimensionInput : HTMLInputElement = !!document.getElementById "arrayDimension"
        int dimensionInput.value

/// Set up the events for the start and stop button
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