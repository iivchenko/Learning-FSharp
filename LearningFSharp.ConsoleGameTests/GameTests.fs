// TODO: Read https://fsharpforfunandprofit.com/posts/inheritance/
// TODO: Read https://fsharpforfunandprofit.com/posts/function-signatures/
namespace LearningFSharp.ConsoleGameTests

open NUnit.Framework
open LearningFSharp.ConsoleGame

type TestGame(update, draw) =
    inherit Game()
    
    member private this._update = update
    member private this._draw = draw
    
    override this.Update() =
        this._update()

    override this.Draw() =
        this._draw()

[<TestFixture>]
type GameTests =

    [<Test>]
    member this.``Some test``() =
        
        let update = fun() -> printfn "update"

        let draw = fun() -> printfn "draw"

        let game = new TestGame(update, draw)
        game.Update()
    
