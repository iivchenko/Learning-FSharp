// TODO: Read https://fsharpforfunandprofit.com/posts/inheritance/
// TODO: Read https://fsharpforfunandprofit.com/posts/function-signatures/
namespace LearningFSharp.ConsoleGameTests

open FsUnit
open NUnit.Framework
open LearningFSharp.ConsoleGame
open System.Threading.Tasks

[<TestFixture>]
type GameTests() =

    [<Test>]
    member this.``Some test``() =
        
        let update = fun() -> printfn "update"

        let draw = fun() -> 
            printfn "draw"
            

        let game = new Game(update, draw)
        let exit = 
             System.Threading.Thread.Sleep(3000)
             game.Exit()

        let task = Task.Run(fun() -> exit)

        game.Run()

