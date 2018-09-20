namespace LearningFSharp.Bytecode
    
    open LearningFSharp.TypeModule

    type IStack =
        abstract Push : Value -> unit
        abstract Pop : unit -> Value
        abstract IsEmpty : bool with get

    type Stack =
        val mutable _stack : Value list

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

            override this.IsEmpty with get () = this._stack.IsEmpty

