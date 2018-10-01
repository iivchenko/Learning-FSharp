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

            match code.Trim() with 
            | Command { Name = _; Param1 = None ; Param2 = None }
            | Command { Name = _; Param1 = Some _; Param2 = Some _ } ->
                failwith "Push Command should always have one parameter!"
            | Command { Name = _; Param1 = Some param1; Param2 = None } ->
                push stack param1
            | _ -> failwith <| "Can't resolve command '" + code + "'"

type PopCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "POP"

        member this.Execute (code : string) =
            match code.Trim() with 
            | Command { Name = _; Param1 = None ; Param2 = None } ->
                stack.Pop() |> ignore            
            | Command { Name = _; Param1 = Some _; Param2 = None }
            | Command { Name = _; Param1 = Some _; Param2 = Some _ } ->
                failwith "Pop Command should not have parameters!"
            | _ -> failwith <| "Can't resolve command '" + code + "'"

type AddCommand(stack : IStack) =
    interface ICommand with
        member this.CanExecute (code : string) =
            code.IsCommand "ADD"

        member this.Execute (code : string) =
            //applyBinary stack (+)

            match code.Trim() with 
            | Command { Name = _; Param1 = None ; Param2 = None } -> ()
            | Command { Name = _; Param1 = Some param1; Param2 = None } ->
                push stack param1
            | Command { Name = _; Param1 = Some param1; Param2 = Some param2 } ->
                push stack param2
                push stack param1
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 + value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) + value2)
            | Int value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | Double value1, Double value2 -> stack.Push <| Double (value1 + value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 + (double value2))
            | Double value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, String value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, Int value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))
            | String value1, Double value2 -> stack.Push <| String (String.Format("{0}{1}", value1, value2))

type SubstractCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "SUB"

        member this.Execute (code : string) =
            //applyBinary stack (-)

            match code.Trim() with 
            | Command { Name = _; Param1 = None; Param2 = None } -> ()
            | Command { Name = _; Param1 = Some param1; Param2 = None } ->
                push stack param1
            | Command { Name = _; Param1 = Some param1; Param2 = Some param2 } ->
                push stack param2
                push stack param1
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 - value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) - value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 - value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 - (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type MultiplyCommand(stack : IStack) =
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "MUL"

        member this.Execute (code : string) =
            //applyBinary stack (*)

            match code.Trim() with 
            | Command { Name = _; Param1 = None; Param2 = None } -> ()
            | Command { Name = _; Param1 = Some param1; Param2 = None } ->
                push stack param1
            | Command { Name = _; Param1 = Some param1; Param2 = Some param2 } ->
                push stack param2
                push stack param1
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 * value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) * value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 * value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 * (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type DivideCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) = 
            code.IsCommand "DIV"

        member this.Execute (code : string) = 
            //applyBinary stack (/)
            
            match code.Trim() with 
            | Command { Name = _; Param1 = None; Param2 = None } -> ()
            | Command { Name = _; Param1 = Some param1; Param2 = None } ->
                push stack param1
            | Command { Name = _; Param1 = Some param1; Param2 = Some param2 } ->
                push stack param2
                push stack param1
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 / value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) / value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 / value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 - (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

type PrintCommand(stack : IStack) = 
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand "PRNT"

        member this.Execute (code : string) =
            match stack.Pop() with
            | Int value -> printf "%i" value
            | Double value -> printf "%f" value
            | String value -> printf "%s" value