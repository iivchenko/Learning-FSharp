namespace LearningFSharp.Bytecode.SmallAssembler.Tests

open FsUnit
open LearningFSharp.Bytecode.SmallAssembler
open LearningFSharp.Bytecode
open NUnit.Framework
open System

[<TestFixture>]
type InitInitializeCommandTests() =
    
    [<TestCase("\"\"", "")>]
    [<TestCase("\"Hello my friend!\"", "Hello my friend!")>]
    [<TestCase("\" Hello my friend! \"", " Hello my friend! ")>]
    member this.``InitializeCommand. StringItem test cases. Success Case``(input: string, expected: string) =
        let stack = new Stack() :> IStack
        let command = new InitializeCommand(stack) :> ICommand

        command.Execute("INIT " + input)

        stack.Pop() |> should equal (StringItem expected)

    [<TestCase("\"")>]
    [<TestCase(null)>]
    member this.``InitializeCommand. All items test cases. Fail Case``(input: string) =
        let stack = new Stack() :> IStack
        let command = new InitializeCommand(stack) :> ICommand
        
        (fun () -> command.Execute("INIT " + input) |> ignore) |> should (throwWithMessage <| "There no converter for the '" + input + "' value!") typeof<Exception>