module LearningFSharp.Bytecode.SmallAssembler.CommandModule
    
    open System
    open LearningFSharp.ConvertModule
    open LearningFSharp.Bytecode
    open LearningFSharp.TypeModule
    open System.Text.RegularExpressions
    
    type System.String with 
        member this.IsCommand (command : string) = 
            this.StartsWith(command, StringComparison.OrdinalIgnoreCase)

    let push (stack : IStack) value =
         match value with 
            | TryInt x -> stack.Push (Int x)
            | TryDouble x-> stack.Push (Double x)
            | TryString x -> stack.Push (String x)
            | x -> failwith ("There no converter for the '" + x + "' value!")

    let (|CommandNoParams|CommandOneParam|CommandTwoParams|UndefinedCommand|) (command:string) =
        let commandPattern = "^(?<command>[A-Za-z]+)$"
        let commandAndOneParamPattern = "^(?<command>[A-Za-z]+) (?<param1>([^,]+|\"[^,]+\"))$" 
        let commandAndTwoParamPattern = "^(?<command>[A-Za-z]+) (?<param1>([^,]+|\"[^,]+\")),(?<param2>([^,]+|\"[^,]+\"))$"

        match command with 
        | _ when Regex.IsMatch(command, commandPattern) -> 
            let m = Regex.Match(command, commandPattern)
            CommandNoParams (m.Groups.["command"].Value.ToUpper())

        | _ when Regex.IsMatch(command, commandAndOneParamPattern) ->
            let m = Regex.Match(command, commandAndOneParamPattern)
            CommandOneParam (m.Groups.["command"].Value.ToUpper(), m.Groups.["param1"].Value)

        | _ when Regex.IsMatch(command, commandAndTwoParamPattern) ->
            let m = Regex.Match(command, commandAndTwoParamPattern)

            CommandTwoParams (m.Groups.["command"].Value.ToUpper(), m.Groups.["param1"].Value, m.Groups.["param2"].Value)

        | _ -> UndefinedCommand
