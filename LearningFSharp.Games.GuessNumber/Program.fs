// Namespaces
open System

// Modules
open LearningFSharp.Utils.ConsoleModule

let rec gameLoop (random:Random) =
    let rec internalGameLoop number =
        let (|Int|_|) (str : string) =
           match Int32.TryParse(str) with
            | (true,int) -> Some(int)
            | _ -> None

        let rec getGuess () =
            let answer = readLn ()
            match answer with
            | Int i -> i
            | _ -> 
                write <| red "It is not a number!! Try again : "
                getGuess ()   

        let guess = getGuess ()

        match guess with
        | _ when guess = number -> 
            write <| green "Perfect!" + Line
        | _ when guess > number ->
            write <| gray "It should be " + yellow "less" + gray ". Try once more : "
            internalGameLoop number
        | _  -> 
            write <| gray "It should be " + yellow "more" + gray ". Try once more : "
            internalGameLoop number

    let number = random.Next(1, 10)
    write <| gray "Guess number: "

    internalGameLoop number

    write <| gray "Want to start again? (y/n) : "
    
    match readLn () with 
    | "y" | "Y" | "yes" | "YES" -> 
        cls()
        gameLoop random
    | _ -> ()

[<EntryPoint>]
let main argv =
    
    gameLoop <| new Random()

    0
