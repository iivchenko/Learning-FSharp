// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

    // Game class stuff -> screen size and invisible cursors
    Console.SetBufferSize(200, 200)
    Console.CursorVisible <- false

    Console.WriteLine("Hello")

    Console.ForegroundColor <- ConsoleColor.Red

    Console.WriteLine("Hello")

    Console.ForegroundColor <- ConsoleColor.Green

    printfn "Hello World from F#!"
    Console.ReadKey()
    0 // return an integer exit code
