namespace LearningFSharp.Bytecode.Tests

open FsUnit
open LearningFSharp.Bytecode
open LearningFSharp.TypeModule
open NUnit.Framework
open System

[<TestFixture>]
type StackTests() =

    [<Test>]
    member this.``Push & Pop. Stack is empty; Add one item. One item in the stack.``() =
        
        // Arrange 
        let expected = Int 777
        let stack = new Stack() :> IStack
        
        // Act
        stack.Push expected        
        let actual = stack.Pop()

        // Assert
        actual |> should equal expected

    [<Test>]
    member this.``Push & Pop. Stack has one item; Add one item. Two items in the stack.``() =
         
        // Arrange 
        let expected1 = Int 777
        let expected2 = Int 888
        let stack = new Stack() :> IStack
        
        // Act
        stack.Push expected1
        stack.Push expected2
        let actual1 = stack.Pop()
        let actual2 = stack.Pop()

        // Assert
        actual1 |> should equal expected2
        actual2 |> should equal expected1

    [<Test>]
    member this.``Pop. Stack is empty. Throws.``() =
        
        // Arrange 
        let stack = new Stack() :> IStack

        // Act & Assert
        (fun () -> stack.Pop() |> ignore) |> should (throwWithMessage "Stack is empty!") typeof<Exception>
