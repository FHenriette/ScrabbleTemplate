namespace Aug_2019
module Sum_Types =
    // Question 1.1
    type Sum<'a, 'b> =
    | Left of 'a
    | Right of 'b

    let x = Left "Test"
    let y = Right 8

    let rec sumMap f g s =
        match s with
        | Left x -> f x
        | Right y -> g y

    //let testSumMap1 = sumMap List.length String.length (Left [1; 2; 3]) // 3
    //let testSumMap2 = sumMap List.length String.length (Right "Hello World!") // 12

    // Question 1.2
    type SumColl<'a, 'b> =
    | Nil
    | CLeft of 'a * SumColl<'a, 'b>
    | CRight of 'b * SumColl<'a, 'b>

    let sumColl = CLeft ([true; false; true], CRight (1, Nil))

    let rec ofList lst =
        match lst with
        | [] -> Nil
        | Left x :: xs -> CLeft (x, ofList xs)
        | Right y :: ys -> CRight (y, ofList ys)

    //let testOfList = ofList [Left "Hello"; Right [1; 2; 3]; Left " world!!!"] // CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))

    // Question 1.3
    let reverse sumColl =
        let rec aux acc sumColl =
            match sumColl with
            | Nil -> acc
            | CLeft (x, y) -> aux (CLeft (x, acc)) y
            | CRight (x, y) -> aux (CRight (x, acc)) y
        aux Nil sumColl

    //let testReverse = reverse (CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))) // CLeft (" world!!!", CRight ([1; 2; 3], CLeft ("Hello", Nil)))

    // Question 1.4
    let lcons ss a = CLeft(a, ss)
    let rcons ss b = CRight(b, ss)

    let ofList2 lst =
          List.foldBack (fun x acc -> sumMap (lcons acc) (rcons acc) x) lst Nil

    // Question 1.5
    let rec foldBackSumColl f g sumColl =
        match sumColl with
        | Nil -> id
        | CLeft (x, y) -> f x << foldBackSumColl f g y
        | CRight (x, y) -> g x << foldBackSumColl f g y

    let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))
    //let testFoldBackSumColl = foldBackSumColl (fun s acc -> String.length s + acc) (fun lst acc -> List.length lst - acc) coll 0 // 7

module Code_Comprehension =
    let f s =
        let l = String.length s
        let rec aux =
            function
            | i when i = l -> []
            | i -> s.[i] :: aux (i + 1)
        aux 0
    
    let g s = 
        s |> f |>
        List.filter System.Char.IsLetter |>
        List.map System.Char.ToLower |>
        fun lst -> lst = List.rev lst

    // What are the types of functions f and g?
    // f : string -> list<char>
    // g : string -> bool
    
    // What do functions f and g do? Focus on what they do rather than how they do it.
    // Function f makes a string into a list of characters from the string
    // Function g checks if a string contains the same letters

    // What would be appropriate names for functions f and g?
    // The function f: stringToCharList
    // The function g: isPalindrome

    // Question 2.2
    let f2 (s:string) = [for c in s do yield c]

    // Question 2.3
    let g2 = 
        f >>
        List.filter System.Char.IsLetter >>
        List.map System.Char.ToLower >>
        fun lst -> lst = List.rev lst

    // Question 2.4
    // Because the list concatenation will be evaluated after last function call.
    let fTail s =
        let l = String.length s
        let rec aux c =
            function
            | i when i = l -> c []
            | i -> aux (fun res -> c (s.[i] :: res)) (i + 1)
        aux id 0

    // Question 2.5
    let gOpt (s : string) = 
        let rec aux i j =
            match j with
            | j when i >= j -> true
            | j when not (System.Char.IsLetter s.[i]) ->
                aux (i + 1) j
            | j when not (System.Char.IsLetter s.[j]) ->
                aux i (j - 1)
            | j when System.Char.ToLower s.[i] = System.Char.ToLower s.[j] ->
                aux (i + 1) (j - 1)
            | _ -> false
        aux 0 (String.length s-1)