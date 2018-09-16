namespace LearningFSharp.Bytecode

    type public ICommand =
        abstract CanExecute : string -> bool
        abstract Execute : string -> unit