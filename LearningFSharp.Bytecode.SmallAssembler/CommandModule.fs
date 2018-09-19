module LearningFSharp.Bytecode.SmallAssembler.CommandModule
    
    open System
    open LearningFSharp.ConvertModule
    open LearningFSharp.Bytecode    
    
    type System.String with 
        member this.IsCommand (command : string) = 
            this.StartsWith(command, StringComparison.OrdinalIgnoreCase)

    let push (stack : IStack) value =
         match value with 
            | TryInt x -> stack.Push (IntItem x)
            | TryFloat x-> stack.Push (FloatItem x)
            | TryString x -> stack.Push (StringItem (x))
            | x -> failwith ("There no converter for the '" + x + "' value!")
    
    //let applyUnary (stack : IStack) action = 
    //    match stack.Pop() with
    //    | IntItem value1 -> action value1
    //    | FloatItem value1 -> action value1)
    //    | StringItem value1 -> StringItem (String.Format("{0}", value1))        

    //let applyBinary (stack : IStack) (action : 'a -> 'a -> 'a) =
    //    match stack.Pop(), stack.Pop() with
    //    | IntItem value1, IntItem value2 -> stack.Push <| IntItem (action value1 value2)
    //    | FloatItem value1, FloatItem value2 -> stack.Push <| FloatItem (action value1 value2)
    //    | StringItem value1, StringItem value2 -> stack.Push <| StringItem (String.Format("{0}{1}", value1, value2))        
