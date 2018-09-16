namespace LearningFSharp.Bytecode

    type IStack =
        abstract Push : int -> unit
        abstract Pop : unit -> int

    type Stack =
        val mutable _stack : int list

        new() = { _stack = [] } 

        interface IStack with 
            member this.Push value =
                this._stack <- value::this._stack

            member this.Pop() =
                match this._stack with
                | [] -> failwith "Stack is empty!"
                | head::tail -> 
                    this._stack <- tail
                    head

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
