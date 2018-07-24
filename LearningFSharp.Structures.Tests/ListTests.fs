namespace LearningFSharp.Structures.Tests

open LearningFSharp.Structures
open LearningFSharp.Structures.ListExt
open NUnit.Framework

[<TestFixture>]
type ListTests() = 

    let rec assertEquals (expected:List<'T>) (actual:List<'T>) =
        match expected, actual with 
        | List.Empty, List.Empty -> ()
        | List.Node(x, tailExpected), List.Node(y, tailActual) ->
            assertEquals tailExpected tailActual
            Assert.That(y, Is.EqualTo(x))
        | _, _ -> failwith "Different lists size!"

    [<Test>]
    member this.``Test: 'filter'.``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(2, List.Node(4, List.Empty))
        let actual = filter (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual       

        assertEquals expected actual

    [<Test>]
    member this.``Test: 'map'.``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty))))
        let actual = map (fun x -> x + 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        assertEquals expected actual

    [<Test>]
    member this.``Test: 'first'. If list is EMPTY then return default value``() =

        let initial = List.Empty
        let expected = 0
        let actual = first (fun x -> x > 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return first element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 1
        let actual = first (fun x -> x = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return second element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 2
        let actual = first (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return third element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 3
        let actual = first (fun x -> x / 3 = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. If list is EMPTY then return default value``() =

        let initial = List.Empty
        let expected = 0
        let actual = last (fun x -> x > 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return fourth element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 4
        let actual = last (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return second element of the sequence from the end``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty)))))
        let expected = 4
        let actual = last (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        
        // Act
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return first element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 1
        let actual = last (fun x -> x = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. If list is EMPTY then return default value``() =

        let initial = List.Empty
        let expected = 0
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return first element of the sequence``() =

        let initial = List.Node(4, List.Node(2, List.Node(3, List.Node(1, List.Empty))))
        let expected = 4
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return second element of the sequence``() =

        let initial = List.Node(1, List.Node(5, List.Node(3, List.Node(4, List.Node(2, List.Empty)))))
        let expected = 5
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        
        // Act
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return last element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty)))))
        let expected = 5
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. If list is EMPTY then return default value``() =

        let initial = List.Empty
        let expected = 0
        let actual = min(fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return first element of the sequence``() =

        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 1
        let actual = min (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return second element of the sequence``() =

        let initial = List.Node(2, List.Node(1, List.Node(3, List.Node(4, List.Node(2, List.Empty)))))
        let expected = 1
        let actual = min (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        
        // Act
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return last element of the sequence``() =

        let initial = List.Node(2, List.Node(2, List.Node(3, List.Node(4, List.Node(1, List.Empty)))))
        let expected = 1
        let actual = min (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))