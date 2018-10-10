namespace LearningFSharp.Structures

type List<'T> =
    | Empty  
    | Node of 'T * List<'T>

    static member (+) (x, tail) =
        Node(x, tail)
    static member (+) (tail, x) =
        let rec appendInternal list acc = 
            match list with 
            | Empty -> Node(acc, Empty)
            | Node(x, tail) -> Node(x, (appendInternal tail acc))
        appendInternal tail x
    static member (+) (head, tail) =
        let rec concatInternal list acc = 
            match list with 
            | Empty -> acc
            | Node(x, tail) -> Node(x, (concatInternal tail acc))        
        concatInternal head tail

module public ListExt =

    let rec iter action list  =
        match list with
        | Empty -> ()
        | Node(head, tail) -> 
            action head
            iter action tail

    let filter predicate list =
        let rec filterIn predicate list acc =
            match list with
            | Empty -> acc
            | Node(head, tail) when predicate head -> filterIn predicate tail <| Node(head, acc)
            | Node(_, tail) -> filterIn predicate tail acc
        filterIn predicate list Empty
        
    let rec map (transform:'a -> 'a) (list:List<'a>) =
        let myId (x:List<'a>) = x
        let cons (x:'a) (list:List<'a>) = Node(x, list)
        let rec mapIn (transform:'a -> 'a) (list:List<'a>) (acc:List<'a> -> List<'a>) =
            match list with
            | Empty -> acc Empty
            | Node(head, tail) ->
                let acc2 x y = acc (cons x y)
                mapIn transform tail (acc2 <| transform head)
        mapIn transform list myId

    let rec map2 transform list =
        let cons x list = Node(x, list)
        let rec mapIn transform acc = 
            function
            | Empty -> acc Empty
            | Node(head, tail) ->
                mapIn transform (acc << (cons <| transform head)) tail
        mapIn transform id list

    let rec first predicate list =
        match list with
        | Empty -> Unchecked.defaultof<'T>
        | Node(head, _) when predicate head -> head
        | Node(_, tail) -> first predicate tail

    let last predicate list =
        let rec realyLast predicate list2 acc =
            match list2 with
            | Empty -> acc
            | Node(head, tail) when predicate head -> realyLast predicate tail head
            | Node(_, tail) -> realyLast predicate tail acc
        realyLast predicate list Unchecked.defaultof<'T>

    let rec all predicate list =
        match list with 
        | Empty -> true
        | Node(x, tail) when predicate x ->
            all predicate tail
        | Node(_, _) -> false

    let rec any predicate list = 
        match list with 
        | Empty -> false
        | Node(x, _) when predicate x -> true
        | Node(_, tail) ->
            any predicate tail

    let rec max compare list =
        match list with
        | Empty -> Unchecked.defaultof<'T>
        | Node(x, Empty) -> x
        | Node(x, Node(y, tail)) when compare x y = 1 -> max compare (Node(x, tail))
        | Node(_, Node(y, tail)) -> max compare <| (Node(y, tail))

    let rec min compare list =
        match list with
        | Empty -> Unchecked.defaultof<'T>
        | Node(x, Empty) -> x
        | Node(x, Node(y, tail)) when compare x y = -1 -> min compare (Node(x, tail))
        | Node(_, Node(y, tail)) -> min compare <| (Node(y, tail))

    let reverse list =
        let rec realyReverse list2 acc =
            match list2 with
            | Empty -> acc
            | Node(x, tail) -> realyReverse tail (Node(x, acc))
        realyReverse list Empty

    (* TODO *)
    (* Aggregate*)
    (* Average *)

    let rec concat list acc = 
        match list with 
        | Empty -> acc
        | Node(x, tail) -> Node(x, (concat tail acc))

    let rec mapMany transform list =
        match list with
        | Empty -> Empty
        | Node(x, tail) ->
            concat <| map transform x <| mapMany transform tail

    let count predicate list =
        let rec countInternal predicate list acc =
            match list with
            | Empty -> acc
            | Node(x, tail) when predicate x ->
                countInternal predicate tail acc + 1
            | Node(_, tail) ->
                countInternal predicate tail acc
        countInternal predicate list 0 

    (* Sort algorithms *)
    let rec selectionSort compare list =
        (* Finish him!!*)
        match list with
        | Empty -> Empty
        | Node(_, Empty) -> list
        | Node(x, Node(y, tail)) when compare x y = 1 -> 
            Node(x, selectionSort compare <| Node(y, tail))
        | Node(x, Node(y, tail)) -> 
            selectionSort compare <| Node(y, selectionSort compare <| Node(x, tail))

    (* List operators *)   
    let inline (!-) x =
        fun next -> Node(x, next)

    let inline (-!-) x y =
        fun next -> x (Node(y, next))

    let inline (-!) x y =
        x <| Node(y, Empty)

    let inline (!!) x =
        Node(x, Empty)