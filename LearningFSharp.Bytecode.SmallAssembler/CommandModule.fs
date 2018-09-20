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
            | TryFloat x-> stack.Push (Float x)
            | TryString x -> stack.Push (String x)
            | x -> failwith ("There no converter for the '" + x + "' value!")

    let (|Command|_|) (command:string) =
        let pattern = sprintf "(.+ (?<param1>.+)\s{0,1}(?<param2>.+))" // TODO: Improve!
        let m = Regex.Match(command, pattern)

        match m with 
        | m when m.Success ->
            match m.Groups with
            | groups when groups.["0"].Success && groups.["param1"].Success &&  groups.["param2"].Success ->            
                Some (Some m.Groups.["param1"].Value, Some m.Groups.["param2"].Value)
            | groups when  groups.["0"].Success && groups.["param1"].Success ->
                Some (Some m.Groups.["param1"].Value, None)
            | groups when groups.["0"].Success -> Some (None, None)
            | _ -> None
        | _ -> failwith "FUCN"
    
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
