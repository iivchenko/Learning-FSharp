module public MyList

type List<'T> =
    | Node of 'T * List<'T>
    | Empty    
    static member (+) (x, y) =
        Node(x, y)

let rec iter action list  =
    match list with
    | Empty -> ()
    | Node(head, tail) -> 
        action head
        iter action tail

let rec filter predicate list =
    match list with
    | Empty -> Empty
    | Node(head, tail) when predicate head ->  head + filter predicate tail
    | Node(_, tail) -> filter predicate tail

let rec map transform list =
    match list with
    | Empty -> Empty
    | Node(head, tail) ->
        transform head + map transform tail

let rec first predicate list =
    match list with
    | Empty -> Unchecked.defaultof<'T>
    | Node(head, tail) when predicate head -> head
    | Node(_, tail) -> first predicate tail

let last predicate list =
    let rec realyLast predicate list2 acc =
        match list2 with
        | Empty -> acc
        | Node(head, tail) when predicate head -> realyLast predicate tail head
        | Node(_, tail) -> realyLast predicate tail acc
    realyLast predicate list Unchecked.defaultof<'T>

let rec max compare list =
    match list with
    | Empty -> Unchecked.defaultof<'T>
    | Node(x, Empty) -> x
    | Node(x, Node(y, tail)) when compare x y = 1 -> max compare <| x + tail
    | Node(x, Node(y, tail)) -> max compare <| y + tail

let rec min compare list =
    match list with
    | Empty -> Unchecked.defaultof<'T>
    | Node(x, Empty) -> x
    | Node(x, Node(y, tail)) when compare x y = -1 -> min compare <| x + tail
    | Node(x, Node(y, tail)) -> min compare <| y + tail

let reverse list =
    let rec realyReverse list2 acc =
        match list2 with
        | Empty -> acc
        | Node(x, tail) -> realyReverse tail <| x + acc
    realyReverse list Empty

let sort compare list =
    (* Finish him!!*)


let inline (!-) x =
    fun next -> Node(x, next)

let inline (-!-) x y =
    fun next -> x (Node(y, next))

let inline (-!) x y =
    x <| Node(y, Empty)


[<EntryPoint>]
let main args =

    let list1 = !- 1 -!- 2 -!- 3 -!- 7 -!- 4 -! 5   
    
    //iter (fun x -> (printfn "%i" x)) list1

    let list2 = 1 + list1
    
    printf "Initial "
    iter (fun x -> (printf "%i " x)) list2

    printfn ""

    printf "Map and Filtered "

    let list3 =
        list2 
        |> map (fun x -> x + 1)
        |> filter (fun x -> x % 2 = 0)
    
    iter (fun x -> (printf "%i " x)) list3
    
    printfn ""

    printf "Reverse "
    list3
    |> reverse
    |> iter (fun x -> (printf "%i " x))

    printfn ""
    
    printfn "First is %i" <| first (fun x -> x % 4 = 0) list3
    printfn "Last is %i" <| last (fun x -> x % 4 = 0) list3
    printfn "Max is %i" <| max (fun x y -> if x > y then 1 else -1) list3
    printfn "Min is %i" <| min (fun x y -> if x > y then 1 else -1) list3
    
    0