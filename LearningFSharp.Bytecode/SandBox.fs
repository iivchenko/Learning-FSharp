namespace LearningFSharp.Bytecode

    type IStack =
        abstract Push : int -> unit
        abstract Pop : unit -> int

    type ICommand =
        abstract Execute : unit -> unit

    type ICommandIdentifier =
        abstract Is : string -> bool

    type ICommandValidator =
        abstract Validate : unit -> unit    

    type IBytecode =
        abstract Execute : string -> unit

    type IBytecodeBuilder =
        abstract Add : ICommand -> IBytecodeBuilder
        abstract Build : unit -> IBytecode
