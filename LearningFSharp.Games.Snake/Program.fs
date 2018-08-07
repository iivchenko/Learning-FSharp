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

let (| Direction |) (direction:Direction) (key:System.ConsoleKey) =
    match key with
    | ConsoleKey.UpArrow when direction = Down -> None
    | ConsoleKey.UpArrow -> Up
    | ConsoleKey.DownArrow when direction = Up  -> None
    | ConsoleKey.DownArrow -> Down
    | ConsoleKey.LeftArrow when direction = Right  -> None
    | ConsoleKey.LeftArrow -> Left
    | ConsoleKey.RightArrow when direction = Left  -> None
    | ConsoleKey.RightArrow -> Right
    | _ -> None

type Snake =
    | Head of Position * Direction * Snake
    | Body of Position * Snake
    | Tail

let draw (x, y) (symbol:char) =
    Console.SetCursorPosition(x, y)
    Console.Write(symbol)

let rec processSnake processHead processBody processTail snake =
    match snake with
    | Head({ X = x; Y = y }, direction, body) ->
        processHead x y
        Head({ X = x; Y = y }, direction, (processSnake processHead processBody processTail body))
    | Body({ X = x; Y = y }, body) ->
        processBody x y
        Body({ X = x; Y = y }, (processSnake processHead processBody processTail body))
    | Tail -> 
        processTail
        Tail

let clearSnake snake =
    processSnake (fun x y -> draw (x, y) ' ') (fun x y -> draw (x, y) ' ') () snake

let drawSnake snake = 
    processSnake (fun x y -> draw (x, y) '0') (fun x y -> draw (x, y) '*') () snake

let updateSnakeDirection snake =
    match snake with
    | Head(position, direction, body) ->
        match Console.ReadKey().Key with 
        | Direction direction Up -> 
            Head(position, Up, body)
        | Direction direction Down  ->
            Head(position, Down, body)
        | Direction direction Left ->
            Head(position, Left, body)
        | Direction direction Right ->
            Head(position, Right, body)
        | _ -> Head(position, direction, body)    
    | Body(_, _)
    | Tail ->
        failwith "Snake should have Head!!"

let moveSnake snake =
    let rec moveInternal (x, y) snake =    
        match snake with
        | Head({ X = px; Y = py }, direction, body) ->
            Head({X = px + x; Y = py + y }, direction, (moveInternal (px, py) body))
        | Body({ X = px; Y = py }, body) ->
            Body({X = x; Y = y }, (moveInternal (px, py) body))
        | Tail -> Tail
    match snake with
    | Head(_, Up, _) ->
        moveInternal (0, -1) snake
    | Head(_, Down, _) ->
        moveInternal (0, 1) snake
    | Head(_, Left, _) ->
        moveInternal (-1, 0) snake
    | Head(_, Right, _) ->
        moveInternal (1, 0) snake
    | Head(_, None, _) -> failwith "Snake should have a Direction!!"
    | Body(_, _)
    | Tail ->
        failwith "Snake should have Head!!"

let rec gameLoop snake speed =
    
    let current = DateTime.Now

    let snake = 
        snake 
            |> clearSnake
            |> updateSnakeDirection
            |> moveSnake
            |> drawSnake
    
    match DateTime.Now.Subtract(current) with
    | elapsed when elapsed + speed > TimeSpan.Zero -> Thread.Sleep(elapsed)
    | _ -> ()

    gameLoop snake speed

[<EntryPoint>]
let main argv =
    Console.CursorVisible <- false

    // TODO:
    // Implement async control input
    // Game over if snake it self
    // Game over if snake move away of the screen
    // Create snake with head only
    // Set snake initiali postion in the center of the screen

    let snake = Head({X = 0; Y = 0}, Down, Body({X = 1; Y = 0}, Body({X = 2; Y = 0},  Body({X = 3; Y = 0}, Tail))))
    
    gameLoop snake <|new TimeSpan(0, 0, 1)
    
    0
