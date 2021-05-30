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

    let testSumMap1 = sumMap List.length String.length (Left [1; 2; 3]) // 3
    let testSumMap2 = sumMap List.length String.length (Right "Hello World!") // 12

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

    let testOfList = ofList [Left "Hello"; Right [1; 2; 3]; Left " world!!!"] // CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))

    // Question 1.3
    let reverse sumColl =
        let rec aux acc sumColl =
            match sumColl with
            | Nil -> acc
            | CLeft (x, y) -> aux y (CLeft (x, acc)) 
            | CRight (x, y) -> aux y (CRight (x, acc))
        aux Nil sumColl

    let testReverse = reverse (CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))) // CLeft (" world!!!", CRight ([1; 2; 3], CLeft ("Hello", Nil)))

    // Question 1.4
    let ofList2 lst = List.fold (fun a b ->
        match b with
        | Left x -> CLeft (x, a)
        | Right y -> CRight (y, a)) Nil 

    // Question 1.5
    let rec foldBackSumColl f g sumColl =
        match sumColl with
        | Nil -> id
        | CLeft (x, y) -> f x << foldBackSumColl f g y
        | CRight (x, y) -> g x << foldBackSumColl f g y

    let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))
    let testFoldBackSumColl = foldBackSumColl (fun s acc -> String.length s + acc) (fun lst acc -> List.length lst - acc) coll 0 // 7