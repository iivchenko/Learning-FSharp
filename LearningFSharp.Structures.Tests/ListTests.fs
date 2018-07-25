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
    member this.``Test: '+'. 'T + List<'T>.``() =

        // Arrange
        let value = 1
        let tail = List.Node(2, List.Node(3, List.Empty))
        let expected = List.Node(1, List.Node(2,List.Node(3, List.Empty)))

        // Act
        let actual = value + tail

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: '+'. List<'T> + 'T .``() =

        // Arrange
        let value = 3
        let head = List.Node(1, List.Node(2, List.Empty))
        let expected = List.Node(1, List.Node(2,List.Node(3, List.Empty)))

        // Act
        let actual = head + value

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: '+'. List<'T> + List<'T> .``() =

        // Arrange      
        let head = List.Node(1, List.Node(2, List.Empty))
        let tail = List.Node(3, List.Node(4, List.Empty))
        let expected = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))

        // Act
        let actual = head + tail

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: list creation operators``()=
        let expected = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let actual = !- 1 -!- 2 -!- 3 -! 4

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'filter'. If list is EMPTY then return empty list.``() =

        // Arrange
        let initial = List.Empty
        let expected = List.Empty

        // Act
        let actual = filter (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'filter'. Return filtered list.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(2, List.Node(4, List.Empty))
        
        // Act
        let actual = filter (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual       

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'map'. If list is EMPTY then return empty list.``() =

        // Arrange
        let initial = List.Empty
        let expected = List.Empty
        
        // Act
        let actual = map (fun x -> x + 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'map'. Returned maped list.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty))))
        
        // Act
        let actual = map (fun x -> x + 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'first'. If list is EMPTY then return default value.``() =

        // Arrange
        let initial = List.Empty
        let expected = 0
        
        // Act
        let actual = first (fun x -> x > 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return first element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 1
        
        // Act
        let actual = first (fun x -> x = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return second element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 2
        
        // Act
        let actual = first (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'first'. Return third element of the sequence.``() =

        // Assert
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 3
        
        // Act
        let actual = first (fun x -> x / 3 = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. If list is EMPTY then return default value.``() =

        // Arrange
        let initial = List.Empty
        let expected = 0

        // Act
        let actual = last (fun x -> x > 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return fourth element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 4

        // Act
        let actual = last (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return second element of the sequence from the end.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty)))))
        let expected = 4

        // Act
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
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'last'. Return first element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Empty)))
        let expected = 1

        // Act
        let actual = last (fun x -> x = 1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. If list is EMPTY then return default value.``() =

        // Arrange
        let initial = List.Empty
        let expected = 0

        // Act
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return first element of the sequence.``() =
        
        // Arrange
        let initial = List.Node(4, List.Node(2, List.Node(3, List.Node(1, List.Empty))))
        let expected = 4

        // Act
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return second element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(5, List.Node(3, List.Node(4, List.Node(2, List.Empty)))))
        let expected = 5

        // Act
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual

        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'max'. Return last element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Node(5, List.Empty)))))
        let expected = 5

        // Act
        let actual = max (fun x y -> if x > y then 1 else 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. If list is EMPTY then return default value.``() =

        // Arrange
        let initial = List.Empty
        let expected = 0

        // Act
        let actual = min(fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return first element of the sequence.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 1

        // Act
        let actual = min (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return second element of the sequence.``() =

        // Arrange
        let initial = List.Node(2, List.Node(1, List.Node(3, List.Node(4, List.Node(2, List.Empty)))))
        let expected = 1

        // Act
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
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'min'. Return last element of the sequence.``() =

        // Arrange
        let initial = List.Node(2, List.Node(2, List.Node(3, List.Node(4, List.Node(1, List.Empty)))))
        let expected = 1

        // Act
        let actual = min (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'all'. If list is EMPTY then return true.``() =

        // Arrange
        let initial = List.Empty
        let expected = true

        // Act
        let actual = all (fun x -> x = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'all'. All elemets satisfy predicate then return true.``() =

        // Arrange
        let initial = List.Node(2, List.Node(4, List.Node(6, List.Node(8, List.Node(10, List.Empty)))))
        let expected = true

        // Act
        let actual = all (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'all'. All elemets not satisfy predicate then return false.``() =
    
        // Arrange
        let initial = List.Node(1, List.Node(3, List.Node(5, List.Node(7, List.Node(9, List.Empty)))))
        let expected = false

        // Act
        let actual = all (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'all'. One elemets not satisfy predicate then return false.``() =

        // Arrange
        let initial = List.Node(2, List.Node(4, List.Node(1, List.Node(8, List.Node(10, List.Empty)))))
        let expected = false

        // Act
        let actual = all (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'any'. If list is EMPTY then return false.``() =

        // Arrange
        let initial = List.Empty
        let expected = false

        // Act
        let actual = any (fun x -> x = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'any'. All elemets satisfy predicate then return true.``() =

        // Arrange
        let initial = List.Node(2, List.Node(4, List.Node(6, List.Node(8, List.Node(10, List.Empty)))))
        let expected = true

        // Act
        let actual = any (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'any'. All elemets not satisfy predicate then return false.``() =

        // Arrange
        let initial = List.Node(1, List.Node(3, List.Node(5, List.Node(7, List.Node(9, List.Empty)))))
        let expected = false

        // Act
        let actual = any (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'any'. One elemet satisfy predicate then return true.``() =

        // Arrange
        let initial = List.Node(2, List.Node(4, List.Node(1, List.Node(8, List.Node(10, List.Empty)))))
        let expected = true

        // Act
        let actual = any (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%b" expected

        printfn ""

        printf "Actual value : "
        printf "%b" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'reverse'. If list is EMPTY then return empty list.``() =

        // Arrange
        let initial = List.Empty
        let expected = List.Empty

        // Act
        let actual = reverse initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'reverse'. Return reversed list.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(4, List.Node(3, List.Node(2, List.Node(1, List.Empty))))
        
        // Act
        let actual = reverse initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual       

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'count'. If list is EMPTY then return zero.``() =

        // Arrange
        let initial = List.Empty
        let expected = 0

        // Act
        let actual = count (fun x -> true) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'count'. Return count of all list.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 4

        // Act
        let actual = count (fun x -> true) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'count'. Return count by predicate.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = 2

        // Act
        let actual = count (fun x -> x % 2 = 0) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected value : "
        printf "%i" expected

        printfn ""

        printf "Actual value : "
        printf "%i" actual
        
        // Assert
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.``Test: 'selectionSort'. If list is EMPTY then return empty list.``() =

        // Arrange
        let initial = List.Empty
        let expected = List.Empty
        
        // Act
        let actual = selectionSort (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    [<Test>]
    member this.``Test: 'selectionSort'. Return selected list.``() =

        // Arrange
        let initial = List.Node(1, List.Node(2, List.Node(3, List.Node(4, List.Empty))))
        let expected = List.Node(4, List.Node(3, List.Node(2, List.Node(1, List.Empty))))
        
        // Act
        let actual = selectionSort (fun x y -> if x > y then 1 else -1) initial

        printf "Initial list: "
        iter (fun x -> (printf "%i " x)) initial

        printfn ""

        printf "Expected list: "
        iter (fun x -> (printf "%i " x)) expected

        printfn ""

        printf "Actual list: "
        iter (fun x -> (printf "%i " x)) actual

        // Assert
        assertEquals expected actual

    // TODO: Finish
    [<Test>]
    member this.test111()=
        let list = !! 1 + 2 + 3 + 4

        iter (fun x -> (printf "%i " x)) list