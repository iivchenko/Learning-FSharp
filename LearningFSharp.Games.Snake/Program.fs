// Learn more about F# at http://fsharp.org

open System
open System.Threading

type Position =
    {
        X: int;
        Y: int;
    }

type Direction =
    | Up
    | Down
    | Left
    | Right
    | None

type Snake =
    | Head of Position * Direction * Snake
    | Body of Position * Snake
    | Tail

let draw (x, y) (symbol:char) =
    Console.SetCursorPosition(x, y)
    Console.Write(symbol)

let rec drawSnake snake drawHead drawBody =
    match snake with
    | Head({ X = x; Y = y }, direction, body) ->
        drawHead x y
        drawSnake body drawHead drawBody
    | Body({ X = x; Y = y }, body) -> 
        drawBody x y
        drawSnake body drawHead drawBody
    | Tail -> ()

let canUpdateDirection direction snake =
    match snake with
    | Head({X = px; Y = py}, _, Body({X = bx; Y = by},_)) ->
        match direction with 
        | Up -> 
            match py - 1 = by with
            | true -> false
            | false -> true
        | Down -> 
            match py + 1 = by with
            | true -> false
            | false -> true
        | Left -> 
            match px - 1 = bx with
            | true -> false
            | false -> true
        | Right -> 
            match px + 1 = bx with
            | true -> false
            | false -> true
        | None -> false
    | _ -> true

let updateDirection direction snake =
    match snake with
    | Head(postion, _, tail) ->
        Head(postion, direction, tail)

// TODO: Apply Direction
let rec moveSnake (x, y) snake =
    match snake with
    | Head({ X = px; Y = py }, direction, body) ->
        Head({X = px + x; Y = py + y }, direction, (moveSnake (px, py) body))
    | Body({ X = px; Y = py }, body) ->
        Body({X = x; Y = y }, (moveSnake (px, py) body))
    | Tail -> Tail

let rec gameLoop snake speed =
    let current = DateTime.Now

    let direction = 
        match Console.ReadKey().Key with
        | ConsoleKey.UpArrow -> Up
        | ConsoleKey.DownArrow -> Down
        | ConsoleKey.LeftArrow -> Left
        | ConsoleKey.RightArrow -> Right
        | _ -> None

    drawSnake snake (fun x y -> draw (x, y) ' ') (fun x y -> draw (x, y) ' ')

    match canUpdateDirection direction snake with
    | true -> snake = updateDirection direction snake     

    let snake = if canUpdateDirection direction snake then updateDirection direction snake else snake
    let snake = snake |> moveSnake

    drawSnake newSnake (fun x y -> draw (x, y) '0') (fun x y -> draw (x, y) '*')
    
    match DateTime.Now.Subtract(current) with
    | elapsed when elapsed + speed > TimeSpan.Zero -> Thread.Sleep(elapsed)
    | _ -> ()

    gameLoop newSnake speed

[<EntryPoint>]
let main argv =
    Console.CursorVisible <- false

    // TODO:
    // Avoid Snake moving back
    // Implement async control input
    // Game over if snake it self
    // Game over if snake move away of the screen
    // Create snake with head only
    // Set snake initiali postion in the center of the screen

    let snake = Head({X = 0; Y = 0}, Down, Body({X = 1; Y = 0}, Body({X = 2; Y = 0},  Body({X = 3; Y = 0}, Tail))))
    
    gameLoop snake <|new TimeSpan(0, 0, 1)
    
    0
