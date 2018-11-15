namespace LearningFSharp.AsyncAndParallel.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
type AsyncTests() =
    
    [<Test>]
    member this.``Async test. Wait async. Some should be changed in time``() =
        let mutable some = false
        
        async {

            // Both async calls are equivalent
            do! Async.Sleep 100
            Async.Sleep 100 |> Async.RunSynchronously

            some <- true
        } |> Async.RunSynchronously

        some |> should equal true

    [<Test>]
    member this.``Async test. Don't wait async. Some should not be changed in time``() =
        let mutable some = false

        async {

            // Both async calls are equivalent
            do! Async.Sleep 100
            Async.Sleep 100 |> Async.RunSynchronously

            some <- true
        } |> Async.Start

        some |> should equal false

    [<Test>]
    member this.``Async test. Don't wait async but sleep in main thread. Magic, some should be changed in time``() =
        let mutable some = false
        
        async {

            // Both async calls are equivalent
            do! Async.Sleep 100
            Async.Sleep 100 |> Async.RunSynchronously

            some <- true
        } |> Async.Start

        Async.Sleep 300 |> Async.RunSynchronously

        some |> should equal true

    member this.AsyncMethod()=
        async{
            do! Async.Sleep 100
        }