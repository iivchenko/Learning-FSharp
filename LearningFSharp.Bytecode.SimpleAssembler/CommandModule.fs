module LearningFSharp.Bytecode.SimpleAssembler.CommandModule
    open System

    type System.String with 
        member this.IsCommand (command : string) = 
            this.StartsWith(command, StringComparison.OrdinalIgnoreCase)

