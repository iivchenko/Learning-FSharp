namespace LearningFSharp.Bytecode
    open System

    type StackItem =
        | IntItem of Int32
        | FloatItem of Single
        | StringItem of String

    type IStack =
        abstract Push : StackItem -> unit
        abstract Pop : unit -> StackItem

    type Stack =
        val mutable _stack : StackItem list

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

