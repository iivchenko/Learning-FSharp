namespace LearningFSharp

module public Math =
    
    (* Factorial function *)
    let rec (!) x =
        match x with
        | _ when x <= 1 -> 1
        | _             -> x + !(x - 1)

    let (!=) x y =
        x <> y

(* TODO: Tail recurstion?? *)
//let rec fib x =
//    match x with 
//    | 0 -> 0
//    | 1 -> 1
//    | _ -> x + fib(x - 1)