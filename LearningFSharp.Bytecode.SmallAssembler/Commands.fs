namespace LearningFSharp.Bytecode.SmallAssembler

open System
open LearningFSharp.Bytecode
open LearningFSharp.TypeModule
open CommandModule

[<Command>]
type PushCommand(stack : IStack) =
    
    [<Literal>]
    let Command = "PUSH"

    interface ICommand with
            
        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            
            match code.Trim() with 
            | CommandNoParams (Command)
            | CommandTwoParams (Command, _, _) ->
                failwith <| Command + " Command should always have one parameter!"
            | CommandOneParam ("PUSH", param1) ->
                push stack param1
            | UndefinedCommand
            | _ -> failwith <| "Can't resolve command '" + code + "'"

[<Command>]
type PopCommand(stack : IStack) =
    
    [<Literal>]
    let Command = "POP"

    interface ICommand with
    
        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            match code.Trim() with 
            | CommandNoParams (Command) ->
                stack.Pop() |> ignore            
            | CommandOneParam (Command, _) 
            | CommandTwoParams (Command, _, _) ->
                failwith <| Command + " Command should not have parameters!"
            | UndefinedCommand
            | _ -> failwith <| "Can't resolve command '" + code + "'"

[<Command>]
type AddCommand(stack : IStack) =
    
    [<Literal>]
    let Command = "ADD"

    interface ICommand with

        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            //applyBinary stack (+)

            match code.Trim() with 
            | CommandNoParams (Command) -> ()
            | CommandOneParam (Command, param1)  ->
                push stack param1
            | CommandTwoParams (Command, param1, param2) ->
                push stack param2
                push stack param1
            | UndefinedCommand
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

[<Command>]
type SubstractCommand(stack : IStack) =
    
    [<Literal>]
    let Command = "SUB"

    interface ICommand with 

        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            //applyBinary stack (-)

            match code.Trim() with 
            | CommandNoParams (Command) -> ()
            | CommandOneParam (Command, param1) ->
                push stack param1
            | CommandTwoParams (Command, param1, param2) ->
                push stack param2
                push stack param1
            | UndefinedCommand
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 - value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) - value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 - value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 - (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

[<Command>]
type MultiplyCommand(stack : IStack) =
    
    [<Literal>]
    let Command = "MUL"

    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            //applyBinary stack (*)

            match code.Trim() with 
            | CommandNoParams (Command) -> ()
            | CommandOneParam (Command, param1) ->
                push stack param1
            | CommandTwoParams (Command, param1, param2) ->
                push stack param2
                push stack param1
            | UndefinedCommand
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 * value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) * value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 * value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 * (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

[<Command>]
type DivideCommand(stack : IStack) = 
    
    [<Literal>]
    let Command = "DIV"

    interface ICommand with 
        member this.CanExecute (code : string) = 
            code.IsCommand Command

        member this.Execute (code : string) = 
            //applyBinary stack (/)
            
            match code.Trim() with 
            | CommandNoParams (Command) -> ()
            | CommandOneParam (Command, param1) ->
                push stack param1
            | CommandTwoParams (Command, param1, param2) ->
                push stack param2
                push stack param1
            | UndefinedCommand
            | _ -> failwith <| "Can't resolve command '" + code + "'"

            match stack.Pop(), stack.Pop() with
            | Int value1, Int value2 -> stack.Push <| Int (value1 / value2)
            | Int value1, Double value2 -> stack.Push <| Double ((double value1) / value2)
            | Double value1, Double value2 -> stack.Push <| Double (value1 / value2)
            | Double value1, Int value2 -> stack.Push <| Double (value1 - (double value2))
            | t1, t2 -> failwith <| System.String.Format("Unsupported types: {0}{1}", t1.ToString(), t2.ToString())

[<Command>]
type PrintCommand(stack : IStack) = 
    [<Literal>]
    let Command = "PRNT"
    
    interface ICommand with 
        member this.CanExecute (code : string) =
            code.IsCommand Command

        member this.Execute (code : string) =
            match stack.Pop() with
            | Int value -> printf "%i" value
            | Double value -> printf "%f" value
            | String value -> printf "%s" value