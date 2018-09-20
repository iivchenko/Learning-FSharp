namespace LearningFSharp.Bytecode.SmallAssembler

open System
open LearningFSharp.Bytecode
open LearningFSharp.TypeModule
open CommandModule

type PushCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "PUSH"

        member this.Execute (code : string) =
            code.Substring("PUSH ".Length) |> push stack 

type PopCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "POP"

        member this.Execute (code : string) =
            stack.Pop() |> ignore

type AddCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "ADD"

        member this.Execute (code : string) =
            //applyBinary stack (+)

            match code.Trim() with 
            | Command (None, None) -> ()
            | Command (Some param1, None) ->
                push stack param1
            | Command (Some param1, Some param2) ->
                push stack param1
                push stack param2

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 + value2)
            | Int value1, Float value2 -> stack.Push <| Float ((float32 value1) + value2)
            | Int value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | Float value1, Float value2 -> stack.Push <| Float (value1 + value2)
            | Float value1, Int value2 -> stack.Push <| Float (value1 + (float32 value2))
            | Float value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, Int value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, Float value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))

type SubstractCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "SUB"

        member this.Execute (code : string) =
            //applyBinary stack (-)

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 - value2)
            | Int value1, Float value2 -> stack.Push <| Float ((float32 value1) - value2)
            | Float value1, Float value2 -> stack.Push <| Float (value1 - value2)
            | Float value1, Int value2 -> stack.Push <| Float (value1 - (float32 value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type MultiplyCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "MUL"

        member this.Execute (code : string) =
            //applyBinary stack (*)

             match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 * value2)
            | Int value1, Float value2 -> stack.Push <| Float ((float32 value1) * value2)
            | Float value1, Float value2 -> stack.Push <| Float (value1 * value2)
            | Float value1, Int value2 -> stack.Push <| Float (value1 * (float32 value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type DivideCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) = 
            code.IsCommand "DIV"

        member this.Execute (code : string) = 
            //applyBinary stack (/)

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 / value2)
            | Int value1, Float value2 -> stack.Push <| Float ((float32 value1) / value2)
            | Float value1, Float value2 -> stack.Push <| Float (value1 / value2)
            | Float value1, Int value2 -> stack.Push <| Float (value1 - (float32 value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type PrintCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "PRNT"

        member this.Execute (code : string) =
            match stack.Pop() with
            | Int value -> printf "%i" value
            | Float value -> printf "%f" value
            | String value -> printf "%s" value