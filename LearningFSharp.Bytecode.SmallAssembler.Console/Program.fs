open LearningFSharp.Bytecode.SmallAssembler
open LearningFSharp.Bytecode
open FSharp.CommandLine
open FSharp.CommandLine.Options
open FSharp.CommandLine.Commands
open System.IO
open System.Reflection
open System

let fileOption =
  commandOption {
    names ["f"; "file"]
    description "Path to a script file to execute"
    takes (format("%s").withNames ["file"])
    takes (format("%s").map (fun file -> file))
    suggests (fun _ -> [CommandSuggestion.Files None])
  }

let executeCommand () =
  command {
    name "execute"
    description "Execute script"    
    opt file in fileOption |> CommandOption.zeroOrExactlyOne
    do 
        let stack = new Stack() :> IStack

        let assembly = typeof<CommandAttribute> |> Assembly.GetAssembly

        let commands = 
            assembly.GetTypes()
            |> Array.toList
            |> List.where (fun x -> x.GetCustomAttribute(typeof<CommandAttribute>) <> null) 
            |> List.map (fun x -> Activator.CreateInstance(x, stack) :?> ICommand)

        let bytecode = new Bytecode(commands)

        match file with 
        | Some path -> 
            let data = File.ReadAllLines path
            bytecode.Execute data
        | None -> ()
    return 0
  }

[<EntryPoint>]
let main argv =
    executeCommand() |> Command.runAsEntryPoint argv