namespace LearningFSharp.ConsoleGame

open System

open System.Threading

[<Sealed>]
type Game(update, draw) = 
    // World Matrix
    let mutable _isExit = false

    member private this._update = update
    member private this._draw = draw
    member public this._frameTime = new TimeSpan(int64(1/60 * 1000))    
    
    member this.Run() =
        while not _isExit do
            let start = DateTime.Now
            
            // processInput
            
            this._update()
            this._draw()

            // Flush matrix world from one buffer to the second

            match start + this._frameTime - DateTime.Now with
            | value when value > TimeSpan.Zero -> Thread.Sleep(value)
            | _ -> ()

    member this.Exit() =
        _isExit <- true