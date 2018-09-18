namespace LearningFSharp.Bytecode.SimpleAssembler

open System
open LearningFSharp.Bytecode
open CommandModule

type InitializeCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "INIT"

        member this.Execute (code : string) =
            code.Substring("INIT ".Length) |> push stack 

type AddCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "ADD"

        member this.Execute (code : string) =
            //applyBinary stack (+)

            match stack.Pop(), stack.Pop() with
            | IntItem value1, IntItem value2 -> stack.Push <| IntItem (value1 + value2)
            | FloatItem value1, FloatItem value2 -> stack.Push <| FloatItem (value1 + value2)
            | StringItem value1, StringItem value2 -> stack.Push <| StringItem (String.Format("{0}{1}", value1, value2)) 

type SubstractCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "SUB"

        member this.Execute (code : string) =
            //applyBinary stack (-)

            match stack.Pop(), stack.Pop() with
            | IntItem value1, IntItem value2 -> stack.Push <| IntItem (value1 - value2)
            | FloatItem value1, FloatItem value2 -> stack.Push <| FloatItem (value1 - value2)
            | StringItem value1, StringItem value2 -> stack.Push <| StringItem (String.Format("{0}{1}", value1, value2)) 

type MultiplyCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "MUL"

        member this.Execute (code : string) =
            //applyBinary stack (*)

            match stack.Pop(), stack.Pop() with
            | IntItem value1, IntItem value2 -> stack.Push <| IntItem (value1 * value2)
            | FloatItem value1, FloatItem value2 -> stack.Push <| FloatItem (value1 * value2)
            | StringItem value1, StringItem value2 -> stack.Push <| StringItem (String.Format("{0}{1}", value1, value2)) 

type DivideCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) = 
            code.IsCommand "DIV"

        member this.Execute (code : string) = 
            //applyBinary stack (/)

            match stack.Pop(), stack.Pop() with
            | IntItem value1, IntItem value2 -> stack.Push <| IntItem (value1 / value2)
            | FloatItem value1, FloatItem value2 -> stack.Push <| FloatItem (value1 / value2)
            | StringItem value1, StringItem value2 -> stack.Push <| StringItem (String.Format("{0}{1}", value1, value2)) 

type PrintCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "PRNT"

        member this.Execute (code : string) =
            match stack.Pop() with
            | IntItem value1 -> printf "%i" value1
            | FloatItem value1 -> printf "%f" value1
            | StringItem value1 -> printf "%s" value1 