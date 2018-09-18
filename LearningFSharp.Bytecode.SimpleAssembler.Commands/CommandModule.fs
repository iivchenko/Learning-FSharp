module LearningFSharp.Bytecode.SimpleAssembler.CommandModule
    open System
    open LearningFSharp.Bytecode
    open System.Text.RegularExpressions
    
    type System.String with 
        member this.IsCommand (command : string) = 
            this.StartsWith(command, StringComparison.OrdinalIgnoreCase)

    let (|Int|_|) (value:string) = 
        match Int32.TryParse(value) with
        | true, result -> Some (result)
        | _ -> None

    let (|String|_|) (value:string) = 
        // TODO: Think about providers when I will study it
        match Regex.Match(value, "^\"(?<str>[\s\S]*)\"$") with
        | m when m.Success -> Some m.Groups.["str"].Value
        | _ -> None

    let (|Float|_|) (value:string) = 
        match Single.TryParse(value) with
        | true, result -> Some (result)
        | _ -> None

    let push (stack : IStack) value =
         match value with 
            | Int x -> stack.Push (IntItem x)
            | Float x-> stack.Push (FloatItem x)
            | String x -> stack.Push (StringItem (x))
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
