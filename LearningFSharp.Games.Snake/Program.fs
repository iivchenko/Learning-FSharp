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

let updateSnakeDirection (move: Direction option) snake =
    match move with
    | Some newDirection ->
        match snake with
        | Head(position, direction, body) ->
            match direction, newDirection with 
            | Up, Up ->
                snake
            | Down, Down ->
                snake
            | Left, Left ->
                snake
            | Right , Right ->
                snake
            | Up, Down->
                snake
            | Down, Up ->
                snake
            | Left , Right ->
                snake
            | Right, Left->
                snake
            | _ -> Head(position, newDirection, body)
        | Body(_, _)
        | Tail ->
            failwith "Snake should have Head!!"
    | _ -> 
        snake

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

[<EntryPoint>]
let main argv =
    Console.CursorVisible <- false

    // TODO:
    // Game over if snake it self
    // Game over if snake move away of the screen
    // Create snake with head only
    // Set snake initiali postion in the center of the screen

    let speed = 250

    let agent = MailboxProcessor.Start(fun inbox -> 
        let rec loop snake = async {

            let time = DateTime.Now

            let! msg = inbox.TryReceive(speed)

            let snake = 
                snake 
                    |> clearSnake
                    |> updateSnakeDirection msg
                    |> moveSnake
                    |> drawSnake

            let diff = TimeSpan.FromMilliseconds(250.0) - time.Subtract(DateTime.Now)

            if diff > TimeSpan.Zero
            then 
                Thread.Sleep((int diff.TotalMilliseconds))
            else 
                ()

            do! loop snake
        }
        
        let snake = Head({X = 0; Y = 0}, Down, Body({X = 1; Y = 0}, Body({X = 2; Y = 0},  Body({X = 3; Y = 0}, Tail))))

        loop snake
    )    

    let rec controlLoop key =
        
        match Console.ReadKey().Key with
        | ConsoleKey.UpArrow when key <> Up ->
            agent.Post(Up)
            controlLoop Up
        | ConsoleKey.UpArrow ->
            controlLoop Up
        | ConsoleKey.DownArrow when key <> Down -> 
            agent.Post(Down)
            controlLoop Down
        | ConsoleKey.DownArrow ->
            controlLoop Down
        | ConsoleKey.LeftArrow when key <> Left -> 
            agent.Post(Left)
            controlLoop Left
        | ConsoleKey.LeftArrow ->
            controlLoop Left
        | ConsoleKey.RightArrow when key <> Right  -> 
            agent.Post(Right)
            controlLoop Right
        | ConsoleKey.RightArrow ->
            controlLoop Right
        | ConsoleKey.Escape ->
            ()
        | _ -> 
            controlLoop None

    controlLoop None
    0
