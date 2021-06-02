namespace May_2020
module Insertion_Sort =
    // Question 1.1
    let rec insert x lst =
        match lst with
        | [] -> [x]
        | y :: ys -> if x < y then x :: y :: ys else y :: (insert x ys)

    let rec insertionSort lst =
        match lst with
        | [] -> []
        | y :: ys -> insertionSort ys |> insert y

    //let testInsert1 = insert true []
    //let testInsert2 = insert 5 [1; 3; 8; 9]
    //let testInsert3 = insert 'c' ['a'; 'd'; 'e']

    //let testInsertionSort1 = insertionSort [5; 3; 1; 8; 9]
    //let testInsertionSort2 = insertionSort ['o'; 'l'; 'H'; 'e'; 'l']
        
    // Question 1.2
    let insertTail x lst =
        let rec aux acc l =
            match l with
            | [] -> List.rev (x :: acc)
            | y :: ys when y < x -> aux (y :: acc) ys
            | y :: ys -> List.rev (y :: x :: acc) @ ys
        aux [] lst

    let insertionSortTail lst =
        let rec aux acc l =
            match l with
            | [] -> acc
            | y :: ys -> aux (insert y acc) ys 
        aux [] lst

    //let testInsertTail1 = insertTail true []
    //let testInsertTail2 = insertTail 5 [1; 3; 8; 9]
    //let testInsertTail3 = insertTail 'c' ['a'; 'd'; 'e']

    //let testInsertionSortTail1 = insertionSortTail [5; 3; 1; 8; 9]
    //let testInsertionSortTail2 = insertionSortTail ['o'; 'l'; 'H'; 'e'; 'l']

    // Question 1.3
    // Why are the higher-order functions from the List library not a good fit to implement insert ?
    // Because the implementation of insert only need to run until the element is placed, where the higher-order functions run through all elements of the List.

    let insertionSort2 lst = List.foldBack insertTail lst []

    //let testInsertionSort2_1 = insertionSort2 [5; 3; 1; 8; 9]
    //let testInsertionSort2_2 = insertionSort2 ['o'; 'l'; 'H'; 'e'; 'l']

    // Question 1.4
    let insertBy f x lst = 
        let rec aux acc l =
            match l with
            | [] -> List.rev (x :: acc)
            | y :: ys when f y < f x -> aux (y :: acc) ys 
            | y :: ys -> (List.rev <| (y :: x :: acc)) @ ys
        aux [] lst

    let insertionSortBy f lst =
        let rec aux acc l =
            match l with
            | [] -> acc
            | y :: ys -> aux (insertBy f y acc) ys
        aux [] lst

    //let testInsertBy = insertBy String.length "abc" ["q"; "bb"; "lbcd"]

    //let testInsertionSortBy1 = insertionSortBy String.length ["bb"; "lbcd"; "q"; "abc"]

module Code_Comprehension =
    // Question 2.1
    let rec foo x =
        function
        | y :: ys when x = y -> ys
        | y :: ys -> y :: (foo x ys)
    let rec bar x =
        function
        | [] -> []
        | xs :: xss -> (x :: xs) :: bar x xss
    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs ->
        let rec aux =
            function
            | [] -> []
            | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
        aux xs

    // What are the types of functions foo , bar , and baz ?
    // foo : 'a -> list<'a> -> list<'a>
    // bar : 'a -> list<list<'a>> -> list<list<'a>>
    // baz : list<'a> -> list<list<'a>

    // What do functions foo , bar , and baz do? Focus on what they do rather than how they do it.
    // foo removes first equal element 
    // bar places the element first in the lists
    // baz order list

    // What would be appropriate names for functions foo , bar , and baz ?
    // foo : removeFirst
    // bar : placeElemFirst
    // baz : orderList

    // Question 2.2
    // Incomplete pattern matches on this expression : Because it doesn't match on the empty
    // Will this cause problems for baz ? : No because baz checks for the empty list
    let rec foo2 x =
        function
        | [] -> [x]
        | y :: ys when x = y -> ys
        | y :: ys -> y :: (foo x ys)

    // Question 2.3
    // What is the type of this expression ? 'a -> (list<'a> -> list<list<'a>>)
    // What does it do? Focus on what it does rather than how it does it. : Permutations

    // Question 2.4
    let bar2 x = List.map (fun xs -> x :: xs)

    // Question 2.5
    let rec baz2 =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs -> 
        let aux y = ((foo y >> baz >> bar y) xs)
        List.collect aux xs

    // Question 2.6
    // foo is not tailrecusive, because the last call is not the function itself but needs concatenated after the function call.
    let fooTail x lst =
        let rec aux c xs =
            match xs with
            | [] -> c []
            | y :: ys when x = y -> c ys
            | y :: ys -> aux (fun res -> c (y :: res)) ys
        aux id lst

    // Question 3.1
    type shape =
        | Rock
        | Scissor
        | Paper

    type result = 
        | Draw
        | PlayerOneWins
        | PlayerTwoWins
    let whatBeats shape =
        match shape with
        | Rock -> Scissor
        | Scissor -> Paper
        | Paper -> Rock
    let rps p1 p2 = 
        if p1 = p2 then Draw
        else 
        let p1Win = whatBeats p1
        if p1Win = p2 then PlayerOneWins
        else PlayerTwoWins

    let p1win = rps Rock Scissor
    let p2win = rps Scissor Rock
    let draw = rps Rock Rock

    // Question 3.2
    type strategy = (shape * shape) list -> shape

    let parrot (s : shape) (moves : (shape * shape) list) =
        match moves with
        | [] -> s
        | (_, s2) :: _ -> s2

    let rock = parrot Rock []
    let scissors = parrot Paper [(Rock, Scissor)]

    let beatingStrat moves =
        if List.isEmpty moves then Rock
        else
        (List.countBy snd >> List.maxBy (fun (f, s) -> (s, -1*String.length (string <| whatBeats f))) >> fst >> whatBeats) moves

    let roundRobin shapes : strategy =
        let mutable left = shapes
        fun moves ->
            if List.isEmpty left then left <- shapes else ()
            let nextShape = left.Head
            left <- left.Tail
            nextShape

    // Question 3.3
    let bestOutOf strat1 strat2 =
        let state = ([], [], (0,0))
        let generator (moves1, moves2, (p1, p2)) =
            let (move1, move2) = strat1 moves1, strat2 moves2
            let moves1 = (move1, move2)::moves1
            let moves2 = (move2, move1)::moves2
            let newscore =
                match rps move1 move2 with
                | PlayerOneWins -> (p1+1, p2)
                | PlayerTwoWins -> (p1, p2+1)
                | Draw -> (p1, p2)
            Some ((p1, p2), (moves1, moves2, newscore))
        Seq.unfold generator state