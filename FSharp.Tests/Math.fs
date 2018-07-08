module public MyMath

type SomeType = 
    | None
    | One
    | Two
    | Three
    | Four

    static member (+) (x, y) =
        match x, y with
        | One, One -> Two
        | One, Two -> Three
        | One, Three -> Four
        | _, _ -> None
    
    static member (+) (x, y) =
        match x, y with
        | 1, One -> Two
        | 2, Two -> Three
        | 3, Three -> Four
        | _, _ -> None

let printT =
    function
    | None -> printf "None"
    | One -> printf "One"
    | Two -> printf "Two"
    | Three -> printf "Three"
    | Four -> printf "Four"
    
(* Factorial function *)
let rec (!) x =
    match x with
    | _ when x <= 1 -> 1
    | _             -> x + !(x - 1)


(* TODO: Tail recurstion?? *)
//let rec fib x =
//    match x with 
//    | 0 -> 0
//    | 1 -> 1
//    | _ -> x + fib(x - 1)

let (!=) x y =
    x <> y

let factorialTest =
    printf "%d" !5

let notEqualsTest = 
    printfn "%b" (2 != 1)
    printfn "%b" (1 != 2)
    printfn "%b" (2 != 2)

// [<Test>]
// member public this.``Overload operators test``()=
    //printfn "%i" <| (-!- 1)

//[<Test>]
//member public this.``Fibonachi values``()=
//    printfn "%i" (fib 0)
//    printfn "%i" (fib 1)
//    printfn "%i" (fib 2)
//    printfn "%i" (fib 3)


//[<EntryPoint>]
let main args =
    
    let a = 2
    let b = Two

    printT <| a + b
    printT <| One + Two

    0