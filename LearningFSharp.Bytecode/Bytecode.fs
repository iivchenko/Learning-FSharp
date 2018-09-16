namespace LearningFSharp.Bytecode

type Bytecode(commands : ICommand list) =
        let execute (code:string) = 
            let command = 
                commands |>
                    List.find (fun x -> x.CanExecute code)

            command.Execute code

        member this.Execute (code : string[]) =

            code 
                |> Array.iter execute

