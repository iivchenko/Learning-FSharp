namespace LearningFSharp.Bytecode.SimpleAssembler

open System
open LearningFSharp.Bytecode
open CommandModule

type InitializeCommand(stack : IStack) =
    interface ICommand with            
        member this.CanExecute (code : string) =
            code.IsCommand "INIT"            

        member this.Execute (code : string) =
            stack.Push <| Int32.Parse(code.Substring("INIT ".Length))

type AddCommand(stack : IStack) =
    interface ICommand with            
        member this.CanExecute (code : string) =
            code.IsCommand "ADD"            

        member this.Execute (code : string) =
            stack.Push <| stack.Pop() + stack.Pop()

type SubstractCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "SUB"

        member this.Execute (code : string) =
            stack.Push <| stack.Pop() - stack.Pop()

type MultiplyCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "MUL"

        member this.Execute (code : string) =
            stack.Push <| stack.Pop() * stack.Pop()

type DivideCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) = 
            code.IsCommand "DIV"

        member this.Execute (code : string) = 
            stack.Push <| stack.Pop() / stack.Pop()

type PrintCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "PRNT"

        member this.Execute (code : string) =
            printfn "%i" <| stack.Pop()