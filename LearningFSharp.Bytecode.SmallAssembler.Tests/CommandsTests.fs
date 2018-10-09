namespace LearningFSharp.Bytecode.SmallAssembler.Tests

open FsUnit
open LearningFSharp
open LearningFSharp.Bytecode
open LearningFSharp.Bytecode.SmallAssembler
open LearningFSharp.TypeModule
open NUnit.Framework
open System

[<TestFixture>]
type CommandsTests() =
    
    [<TestCase("\"\"", "")>]
    [<TestCase("\"Hello my friend!\"", "Hello my friend!")>]
    [<TestCase("\" Hello my friend! \"", " Hello my friend! ")>]
    member this.``PushCommand. StringItem test cases. Success Case``(input: string, expected: string) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand

        command.Execute("PUSH " + input)

        stack.Pop() |> should equal (TypeModule.String expected)

    [<TestCase("0", 0)>]
    [<TestCase("1", 1)>]
    [<TestCase(" 1", 1)>]
    [<TestCase("1 ", 1)>]
    [<TestCase(" 1 ", 1)>]
    member this.``PushCommand. IntItem test cases. Success Case``(input: string, expected: int) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand

        command.Execute("PUSH " + input)

        stack.Pop() |> should equal (TypeModule.Int expected)

    [<TestCase("0.0", 0.0)>]
    [<TestCase("1.1", 1.1)>]
    [<TestCase(" 1.1", 1.1)>]
    [<TestCase("1.1 ", 1.1)>]
    [<TestCase(" 1.1 ", 1.1)>]
    member this.``PushCommand. FloatItem test cases. Success Case``(input: string, expected: double) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand

        command.Execute("PUSH " + input)

        stack.Pop() |> should equal (Value.Double expected)

    [<TestCase("\"")>]    
    member this.``PushCommand. All items test cases. Fail Case``(input: string) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand
        
        (fun () -> command.Execute("PUSH " + input) |> ignore) |> should (throwWithMessage <| "There no converter for the '" + input + "' value!") typeof<Exception>

    [<TestCase(null)>]
    [<TestCase("")>]
    [<TestCase("1,2")>]
    member this.``PushCommand. Invalid number of arguments. Fail``(input: string) =
        let stack = new Stack() :> IStack
        let command = new PushCommand(stack) :> ICommand
        
        (fun () -> command.Execute("PUSH " + input) |> ignore) |> should (throwWithMessage <| "PUSH Command should always have one parameter!") typeof<Exception>

    [<Test>]
    member this.``PopCommand. One item in stack. Item Removed from stack``() =
        let stack = new Stack() :> IStack
        let command = new PopCommand(stack) :> ICommand
        stack.Push <| Int 1
        
        command.Execute("POP")

        stack.IsEmpty |> should be True

    [<TestCase("1")>]
    [<TestCase("1,2")>]
    member this.``PopCommand. Invalid number of arguments. Fail``(input: string) =
        let stack = new Stack() :> IStack
        let command = new PopCommand(stack) :> ICommand

        (fun () -> command.Execute("POP " + input) |> ignore) |> should (throwWithMessage <| "POP Command should not have parameters!") typeof<Exception>

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

    [<Test>]
    member this.``AddCommand. DoubleItem with one parameter. Success Case``() =
        
        let expected = 3.2
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        stack.Push <| Value.Double 1.1

        command.Execute("Add 2.1")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``AddCommand. DoubleItem with two parameter. Success Case``() =
        
        let expected = 3.2
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        command.Execute("Add 1.1,2.1")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``AddCommand. StringItem with one parameter. Success Case``() =
        
        let expected = "Hello man"
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        stack.Push <| Value.String " man"

        command.Execute("Add \"Hello\"")

        stack.Pop() |> should equal (Value.String expected)

    [<Test>]
    member this.``AddCommand. StringItem with two parameter. Success Case``() =
        
        let expected = "Hello man"
        let stack = new Stack() :> IStack
        let command = new AddCommand(stack) :> ICommand

        command.Execute("Add \"Hello\",\" man\"")

        stack.Pop() |> should equal (Value.String expected)

    [<Test>]
    member this.``SubstractCommand. IntItem without parameters. Success Case``() =
        
        let expected = 1
        let stack = new Stack() :> IStack
        let command = new SubstractCommand(stack) :> ICommand

        stack.Push <| Int 2
        stack.Push <| Int 3

        command.Execute("Sub")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``SubstractCommand. IntItem with one parameter. Success Case``() =
        
        let expected = 1
        let stack = new Stack() :> IStack
        let command = new SubstractCommand(stack) :> ICommand

        stack.Push <| Int 2

        command.Execute("Sub 3")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``SubstractCommand. IntItem with two parameter. Success Case``() =
        
        let expected = 1
        let stack = new Stack() :> IStack
        let command = new SubstractCommand(stack) :> ICommand

        command.Execute("Sub 3,2")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``SubstractCommand. DoubleItem with one parameter. Success Case``() =
        
        let expected = 1.0
        let stack = new Stack() :> IStack
        let command = new SubstractCommand(stack) :> ICommand

        stack.Push <| Value.Double 2.1

        command.Execute("Sub 3.1")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``SubstractCommand. DoubleItem with two parameter. Success Case``() =
        
        let expected = 1.0
        let stack = new Stack() :> IStack
        let command = new SubstractCommand(stack) :> ICommand

        command.Execute("Sub 3.1,2.1")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``MultiplyCommand. IntItem without parameters. Success Case``() =
        
        let expected = 6
        let stack = new Stack() :> IStack
        let command = new MultiplyCommand(stack) :> ICommand

        stack.Push <| Int 2
        stack.Push <| Int 3

        command.Execute("Mul")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``MultiplyCommand. IntItem with one parameter. Success Case``() =
        
        let expected = 6
        let stack = new Stack() :> IStack
        let command = new MultiplyCommand(stack) :> ICommand

        stack.Push <| Int 2

        command.Execute("Mul 3")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``MultiplyCommand. IntItem with two parameter. Success Case``() =
        
        let expected = 6
        let stack = new Stack() :> IStack
        let command = new MultiplyCommand(stack) :> ICommand

        command.Execute("Mul 3,2")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``MultiplyCommand. DoubleItem with one parameter. Success Case``() =
        
        let expected = 8.0
        let stack = new Stack() :> IStack
        let command = new MultiplyCommand(stack) :> ICommand

        stack.Push <| Value.Double 2.5

        command.Execute("Mul 3.2")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``MultiplyCommand. DoubleItem with two parameter. Success Case``() =
        
        let expected = 8.0
        let stack = new Stack() :> IStack
        let command = new MultiplyCommand(stack) :> ICommand

        command.Execute("Mul 3.2,2.5")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``DivideCommand. IntItem without parameters. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new DivideCommand(stack) :> ICommand

        stack.Push <| Int 2
        stack.Push <| Int 6

        command.Execute("Div")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``DivideCommand. IntItem with one parameter. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new DivideCommand(stack) :> ICommand

        stack.Push <| Int 2

        command.Execute("Div 6")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``DivideCommand. IntItem with two parameter. Success Case``() =
        
        let expected = 3
        let stack = new Stack() :> IStack
        let command = new DivideCommand(stack) :> ICommand

        command.Execute("Div 6,2")

        stack.Pop() |> should equal (Int expected)

    [<Test>]
    member this.``DivideCommand. DoubleItem with one parameter. Success Case``() =
        
        let expected = 2.5
        let stack = new Stack() :> IStack
        let command = new DivideCommand(stack) :> ICommand

        stack.Push <| Value.Double 3.2

        command.Execute("Div 8.0")

        stack.Pop() |> should equal (Value.Double expected)

    [<Test>]
    member this.``DivideCommand. DoubleItem with two parameter. Success Case``() =
        
        let expected = 2.5
        let stack = new Stack() :> IStack
        let command = new DivideCommand(stack) :> ICommand

        command.Execute("Div 8.0,3.2")

        stack.Pop() |> should equal (Value.Double expected)