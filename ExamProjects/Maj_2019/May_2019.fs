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
        | O when a = O -> O
        | S O -> a
        | S x -> mult a x |> add a

    let rec pow a b =
        match b with
        | O -> S O
        | S O -> a
        | S x -> pow a x |> mult a


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
            | O when a = O -> acc
            | S O -> acc 
            | S x -> aux acc x |> add acc 
        aux O b 
    
    let tailPow a b = 
        let rec aux acc b =
            match b with
            | O -> acc
            | S O -> acc
            | S x -> pow acc x |> mult acc
        aux (S O) b 

    let rec loop f acc p =
        match p with
        | O -> acc
        | S x -> loop f (f acc) x
    
    let loopAdd a b = loop S a b

    let loopMult a b = loop (loopAdd a) O b

    let loopPow a b = loop (loopMult a) (S O) b
    
    let res = toInt(tailPow (fromInt 2u) (fromInt 5u))

module Code_Comprehension = 

    let rec f x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys when x <> y -> 
            match f x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None

    let rec g xs =
        function
        | []    -> xs = []
        | y::ys -> 
            match f y xs with
            | Some xs' -> g xs' ys
            | None     -> false
    
    (*
        val f : x:'a -> _arg1:'a list -> 'a list option when 'a : equality
        val g : xs:'a list -> _arg1:'a list -> bool when 'a : equality

        f: removes first occurence 
        g: checks if the two lists are identical 

        Name for f: removeFirstOccurence 
        Name for g: checkEquality 
    *)

    // You can't have a "when" statements as the last pattern match 

    let rec f2 x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> 
            match f x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None



    let rec fOpt x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> Some (y::ys) |> Option.map (fOpt x ys)
        

    let rec gOpt xs =
        function
        | []    -> xs = []
        | y::ys -> (false, Some xs) |> Option.map (gOpt xs ys) |>  Option.defaultValue
    