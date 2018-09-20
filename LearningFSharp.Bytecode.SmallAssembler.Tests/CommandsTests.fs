namespace LearningFSharp.Bytecode.SmallAssembler.Tests

open FsUnit
open LearningFSharp
open LearningFSharp.Bytecode
open LearningFSharp.Bytecode.SmallAssembler
open LearningFSharp.TypeModule
open NUnit.Framework
open System

[<TestFixture>]
type InitInitializeCommandTests() =
    
    [<TestCase("\"\"", "")>]
    [<TestCase("\"Hello my friend!\"", "Hello my friend!")>]
    [<TestCase("\" Hello my friend! \"", " Hello my friend! ")>]
    member this.``PushCommand. StringItem test cases. Success Case``(input: string, expected: string) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand

        command.Execute("PUSH " + input)

        stack.Pop() |> should equal (TypeModule.String expected)

    [<TestCase("\"")>]
    [<TestCase(null)>]
    member this.``PushCommand. All items test cases. Fail Case``(input: string) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand
        
        (fun () -> command.Execute("PUSH " + input) |> ignore) |> should (throwWithMessage <| "There no converter for the '" + input + "' value!") typeof<Exception>

    [<Test>]
    member this.``PopCommand. One item in stack. Item Removed from stack``() =
        let stack = new Stack() :> IStack        
        let command = new PushCommand(stack) :> ICommand
        stack.Push <| Int 1
        
        command.Execute("POP")

        stack.IsEmpty |> should be True


    [<Test>]
    member this.``AddCommand. IntItem without parameters. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        stack.Push <| Int 1
        stack.Push <| Int 2

        command.Execute("Add")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``AddCommand. IntItem with one parameter. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        stack.Push <| Int 1

        command.Execute("Add 2")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``AddCommand. IntItem with two parameter. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        command.Execute("Add 1,2")

        stack.Pop() |> should equal (Int expected)