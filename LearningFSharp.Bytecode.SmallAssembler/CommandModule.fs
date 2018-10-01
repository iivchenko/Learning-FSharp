module LearningFSharp.Bytecode.SmallAssembler.CommandModule
    
    open System
    open LearningFSharp.ConvertModule
    open LearningFSharp.Bytecode
    open LearningFSharp.TypeModule
    open System.Text.RegularExpressions
    
    type System.String with 
        member this.IsCommand (command : string) = 
            this.StartsWith(command, StringComparison.OrdinalIgnoreCase)

    type Command = 
        { 
            Name : string; 
            Param1 : string Option; 
            Param2 : string Option 
        }

    let push (stack : IStack) value =
         match value with 
            | TryInt x -> stack.Push (Int x)
            | TryDouble x-> stack.Push (Double x)
            | TryString x -> stack.Push (String x)
            | x -> failwith ("There no converter for the '" + x + "' value!")

    let (|Command|_|) (command:string) =
        let commandPattern = "^(?<command>[A-Za-z]+)$"
        let commandAndOneParamPattern = "^(?<command>[A-Za-z]+) (?<param1>([^,]+|\"[^,]+\"))$" 
        let commandAndTwoParamPattern = "^(?<command>[A-Za-z]+) (?<param1>([^,]+|\"[^,]+\")),(?<param2>([^,]+|\"[^,]+\"))$"
        
        match command with 
        | _ when Regex.IsMatch(command, commandPattern) -> 
            let m = Regex.Match(command, commandPattern)

            Some {
                Name = m.Groups.["command"].Value;
                Param1 = None;
                Param2 = None;
            }

        | _ when Regex.IsMatch(command, commandAndOneParamPattern) ->
            let m = Regex.Match(command, commandAndOneParamPattern)

            Some {
                Name = m.Groups.["command"].Value;
                Param1 = Some m.Groups.["param1"].Value;
                Param2 = None;
            }

        | _ when Regex.IsMatch(command, commandAndTwoParamPattern) ->
            let m = Regex.Match(command, commandAndTwoParamPattern)

            Some {
                Name = m.Groups.["command"].Value;
                Param1 = Some m.Groups.["param1"].Value;
                Param2 = Some m.Groups.["param2"].Value;
            }

        | _ -> None
