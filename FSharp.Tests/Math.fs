namespace FSharp.Tests

open NUnit.Framework

[<TestFixture>]
type Math() = 
    
    (* Factorial function *)
    let rec (!) x =
        match x with
        | _ when x <= 1 -> 1
        | _             -> x + !(x - 1)


    let (|||) x = 
        x
    (* TODO: Tail recurstion?? *)
    //let rec fib x =
    //    match x with 
    //    | 0 -> 0
    //    | 1 -> 1
    //    | _ -> x + fib(x - 1)

    let (!=) value1 value2 =
        value1 <> value2

    [<Test>]
    member public this.``Factorial (!) test``()=
        printf "%d" !5

    [<Test>]
    member public this.``Not equals (!=) operator test``()=
        printfn "%b" (2 != 1)
        printfn "%b" (1 != 2)
        printfn "%b" (2 != 2)

    //[<Test>]
    //member public this.``Fibonachi values``()=
    //    printfn "%i" (fib 0)
    //    printfn "%i" (fib 1)
    //    printfn "%i" (fib 2)
    //    printfn "%i" (fib 3)