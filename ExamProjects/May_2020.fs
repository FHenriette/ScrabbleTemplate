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

    let testInsert1 = insert true []
    let testInsert2 = insert 5 [1; 3; 8; 9]
    let testInsert3 = insert 'c' ['a'; 'd'; 'e']

    let testInsertionSort1 = insertionSort [5; 3; 1; 8; 9]
    let testInsertionSort2 = insertionSort ['o'; 'l'; 'H'; 'e'; 'l']
        
    // Question 1.2
    let insertTail x lst =
        let rec aux acc l =
            match l with
            | [] -> List.rev (x :: acc)
            | y :: ys -> if x < y then List.rev (x :: acc) @ ys else aux (y :: acc) ys
        aux [] lst

    let insertionSortTail lst =
        let rec aux acc l =
            match l with
            | [] -> acc
            | y :: ys -> aux (insert y acc) ys 
        aux [] lst

    let testInsertTail1 = insertTail true []
    let testInsertTail2 = insertTail 5 [1; 3; 8; 9]
    let testInsertTail3 = insertTail 'c' ['a'; 'd'; 'e']

    let testInsertionSortTail1 = insertionSortTail [5; 3; 1; 8; 9]
    let testInsertionSortTail2 = insertionSortTail ['o'; 'l'; 'H'; 'e'; 'l']

    // Question 1.3
    // Why are the higher-order functions from the List library not a good fit to implement insert ?
    // Because the implementation of insert only need to run until the element is placed, where the higher-order functions run through all elements of the List.

    let insertionSort2 lst = List.foldBack insertTail lst []

    let testInsertionSort2_1 = insertionSort2 [5; 3; 1; 8; 9]
    let testInsertionSort2_2 = insertionSort2 ['o'; 'l'; 'H'; 'e'; 'l']

    // Question 1.4
    let insertBy f x lst = 
        let rec aux acc l =
            match l with
            | [] -> List.rev (x :: acc)
            | y :: ys -> if f x < f y then List.rev (x :: acc) @ ys else aux (y :: acc) ys
        aux [] lst

    let insertionSortBy f lst =
        let rec aux acc l =
            match l with
            | [] -> acc
            | y :: ys -> aux (insertBy f y acc) ys
        aux [] lst

    let testInsertBy = insertBy String.length "abc" ["q"; "bb"; "lbcd"]

    let testInsertionSortBy1 = insertionSortBy String.length ["bb"; "lbcd"; "q"; "abc"])