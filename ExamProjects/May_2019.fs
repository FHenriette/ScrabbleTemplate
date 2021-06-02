namespace May_2019
module Peano =
    type Peano =
        | O
        | S of Peano

    let toInt (a : Peano)  : uint32 =
        let rec aux x acc =
            match x with
            | O -> acc 
            | S x -> aux x (acc + 1u)
        aux a 0u 

    let x = toInt (S (S (S O))) 

    let fromInt (x:uint32) : Peano = 
        let rec aux z acc = 
            match z with 
            | 0u -> acc 
            | _ -> aux (z - 1u) (S acc)
        aux x O

    let rec add a b = 
        match b with 
        | O -> a 
        | S x -> add (S a) x

    let rec mult a b = 
        match b with 
        | O -> O
        | S O -> a
        | S _ when a = S O -> b
        | S _ when a = O -> O
        | S x -> mult a x |> add a // add a (mult a b)

    let rec pow a b =
        match b with
        | O -> S O
        | S O -> a
        | S x -> pow a x |> mult a // mult a (pow a b)

    let tailAdd a b = 
        let rec aux acc b =
            match b with 
            | O -> acc 
            | S x -> aux (S acc) x 
        aux a b

    let tailMult a b = 
        let rec aux acc b = 
            match b with 
            | O -> acc
            | S O -> a
            | S _ when a = S O -> b
            | S _ when a = O -> acc
            | S x -> aux acc x |> add acc // aux (add a acc) b
        aux O b
    
    let tailPow a b = 
        let rec aux acc b =
            match b with
            | O -> acc
            | S O -> acc
            | S x -> pow (mult a acc) x
        aux (S O) b 

    let rec loop f acc p =
        match p with
        | O -> acc
        | S x -> loop f (f acc) x
    
    let loopAdd a b = loop S a b

    let loopMult a b = loop (loopAdd a) O b

    let loopPow a b = loop (loopMult a) (S O) b

module Code_Comprehension = 
    // Question 2.1
    let rec f x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys when x <> y -> 
            match f x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None

    let rec g xs ys =
        match ys with
        | []    -> xs = []
        | y::ys -> 
            match f y xs with
            | Some xs' -> g xs' ys
            | None     -> false
    
    (*
        'a -> list<'a> -> option<list<'a>>
        list<'a> -> list<'a> -> bool

        f: removes first occurence 
        g: checks if the two lists are identical 

        Name for f: removeFirstOccurence 
        Name for g: isPermutation 
    *)

    // Question 2.2
    // You can't have a "when" statements as the last pattern match 
    let rec f2 x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> 
            match f x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None
    
    // Question 2.3
    let rec fOpt x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> (fOpt x ys) |> Option.map (fun ys' -> (y::ys')) 
            //match f x ys with
            //| Some ys' -> Some (y::ys')
            //| None     -> None

    let rec gOpt xs =
        function
        | []    -> xs = []
        | y::ys -> (fOpt y xs) |> Option.map (fun xs' -> gOpt xs' ys) |> Option.defaultValue false
            //match f y xs with
            //| Some xs' -> g xs' ys
            //| None     -> false

    // Question 2.4
    // f is not tailrecursive because something happens after the function call.

    let fTail x xs =
        let rec aux c lst =
            match lst with
            | []                -> c None
            | y::ys when x = y  -> c (Some ys)
            | y::ys -> aux (fun res -> c (
                                        match res with
                                        | Some ys' -> (Some (y::ys'))
                                        | None     -> (None))) 
                                        ys
        aux id xs
