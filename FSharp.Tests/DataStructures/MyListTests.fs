namespace FSharp.Tests.DataStructures

open NUnit.Framework
open MyList

[<TestFixture>]
type MyListTests() = 

    let rec assertEquals (expected:MyList.List<'T>) (actual:MyList.List<'T>) =
        match expected, actual with 
        | Empty, Empty -> ()
        | Node(x, tailExpected), Node(y, tailActual) ->
            assertEquals tailExpected tailActual
            Assert.That(y, Is.EqualTo(x))
        | _, _ -> failwith "Different lists size!"

    [<Test>]
    member this.`` 'filter' test ``() =

        let initial = MyList.Node(1, MyList.Node(2, MyList.Node(3, MyList.Node(4, MyList.Empty))))
        let expected = MyList.Node(2, MyList.Node(4, MyList.Empty))
        let actual = MyList.filter (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        MyList.iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        MyList.iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        MyList.iter (fun x -> (printf "%i " x)) actual       

        assertEquals expected actual

    [<Test>]
    member this.`` 'map' test ``() =

        let initial = MyList.Node(1, MyList.Node(2, MyList.Node(3, MyList.Node(4, MyList.Empty))))
        let expected = MyList.Node(2, MyList.Node(3, MyList.Node(4, MyList.Node(5, MyList.Empty))))
        let actual = MyList.map (fun x -> x + 1) initial

        printf "Initial list: "
        MyList.iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        MyList.iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        MyList.iter (fun x -> (printf "%i " x)) actual

        assertEquals expected actual

    [<Test>]
    member this.`` 'first' test; If list is EMPTY then return default value``() =

        let initial = MyList.Empty
        let expected = 0
        let actual = MyList.first (fun x -> x > 0) initial

        printf "Initial list: "
        MyList.iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        Assert.That(actual, Is.EqualTo(expected))