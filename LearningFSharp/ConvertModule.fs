module LearningFSharp.ConvertModule
    
    open System
    open System.Text.RegularExpressions

    let (|TryInt|_|) (value:string) = 
        match Int32.TryParse(value) with
        | true, result -> Some (result)
        | _ -> None

    let (|TryString|_|) (value:string) = 
        // TODO: Think about providers when I will study it
        match Regex.Match(value, "^\"(?<body>[\s\S]*)\"$") with
        | m when m.Success -> Some m.Groups.["body"].Value
        | _ -> None

    let (|TryDouble|_|) (value:string) = 
        match Double.TryParse(value) with
        | true, result -> Some (result)
        | _ -> None