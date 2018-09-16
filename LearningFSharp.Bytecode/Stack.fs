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

