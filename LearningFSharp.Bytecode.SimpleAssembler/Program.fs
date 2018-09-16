open LearningFSharp.Bytecode.SimpleAssembler
open LearningFSharp.Bytecode

[<EntryPoint>]
let main argv =
    
    let stack = new Stack() :> IStack
    let commands = 
        [
            (new InitializeCommand(stack) :> ICommand);
            (new PrintCommand(stack) :> ICommand);
            (new AddCommand(stack) :> ICommand);
            (new SubstractCommand(stack) :> ICommand);
            (new MultiplyCommand(stack) :> ICommand);
            (new DivideCommand(stack) :> ICommand);
        ]
    let bytecode = new Bytecode(commands)

    bytecode.Execute argv
    0