namespace LearningFSharp.Tests

open FsUnit
open NUnit.Framework
open LearningFSharp.Math


[<TestFixture>]
type MathTests() =

    [<Test>]
    member this.``Test: '!'.``()=
        
        let expected = 15
        let actual = !5

        actual |> should equal expected

    [<Test>]
    member this.``Test: '!='. Operands are equals then return false.``()=
        
        let expected = false
        let actual = 5 != 5

        actual |> should equal expected

    [<Test>]
    member this.``Test: '!='. Operands are not equals the return true.``()=
        
        let expected = true
        let actual = 5 != 4

        actual |> should equal expected
