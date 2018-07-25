namespace LearningFSharp.ConsoleGame

open System
open System.Threading

[<AbstractClass>]
type Game = 
    // World Matrix

    member private this._frameTime = new TimeSpan(int64(1/60 * 1000))

    abstract member Update: unit -> unit

    abstract member Draw: unit -> unit
    
    member this.Run() =
        while true do
            let start = DateTime.Now            
            // processInput
            this.Update()
            this.Draw()

            // Flush matrix world from one buffer to the second

            Thread.Sleep(start + this._frameTime - DateTime.Now)