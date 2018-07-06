namespace FSharp.Tests

open NUnit.Framework

[<TestFixture>]
type LinqTestsFunctional() = 

    let rec print x =
        match x with
        | [] -> ()
        | head::tail -> 
            printf "%d" head
            print tail

    let rec select transform list =
        match list with 
        | [] -> []
        | head::tail ->
            (transform head)::(select transform tail)

    let rec filter predicate list  =
        match list with 
        | [] -> []        
        | head::tail when predicate head -> head::(filter predicate tail)
        | _::tail -> filter predicate tail
    
    let rec max predicate list =
        match list with
        | [] -> Unchecked.defaultof<'a>
        | [x] -> x
        | [x; y] when predicate x y -> x
        | [_; y] -> y
        | x::y::tail when predicate x y -> max predicate (x::tail)
        | _::y::tail -> max predicate (y::tail)
    
    let rec first predicate list = 
        match list with 
        | [] -> Unchecked.defaultof<'a>
        | [x] when predicate x -> x
        | x::_ when predicate x -> x
        | _::tail -> first predicate tail

    (* TODO: I think I need to study tail recurstion first*)
    //let rec last predicate list = 
    //    match list with
    //    | [] -> Unchecked.defaultof<'a>
    //    | [x] when predicate x -> x
    //    | x::tail when predicate x -> last predicate tail
    //    | _::tail -> last predicate tail

    [<Test>]
    member public _this.``Test 'select' function``() = 

        let input = [0..10]
        let expected = [1..11]

        let actual = select (fun x -> x + 1) input
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member public _this.``Test 'filter' function``() = 

        let input = [0..10]
        let expected = [0; 2; 4; 6; 8; 10]

        let actual = filter (fun x -> x % 2 = 0) input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member public _this.``Test 'max' function``() = 

        (* Arrange *)
        let expected = 50
        let list = [2; 5; -1; 0; expected; 0; -1; 15]        

        (* Act *)
        let actual = max (fun x y -> x > y) list
        
        (* Assert *)
        Assert.That(actual, Is.EqualTo(expected))

    (* User TestCase when I will know how to work with arrays *)
    [<Test>]
    member public _this.``Test 'first' function``() =
        
        let input = [0..10]
        let expected = 5
        
        let actual = first (fun x -> x = expected) input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member public _this.``General purpose LINQ test``() =
        
        let input = [1..10]

        let result = 
            input 
            |> filter (fun x -> x % 2 = 0)
            |> select (fun x -> x + 1)
            |> max (fun x y -> x > y)
        
        printfn "%d" result

        

