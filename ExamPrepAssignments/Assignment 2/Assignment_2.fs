module Assignment_2
    // Exercise 1
    let rec downto1 n = if n > 0 then downto1 (n-1) else []

    let rec downto2 n =
        match n with
        | 0 -> []
        | _ -> downto2 (n-1)

    // Exercise 2
    let rec removeOddIdx xs =
        match xs with
        | [] -> []
        | x :: _ :: xs -> x :: removeOddIdx xs
        | x :: xs -> x :: xs

    // Exercise 3
    let rec combinePair xs =
        match xs with
        | [] -> []
        | [x] -> []
        | x :: y :: xs -> (x, y) :: combinePair xs

    // Exercise 4
    type complex = float * float

    let mkComplex x y = (complex) (x,y)

    let complexToPair (c : complex) =
        match c with
        | (r, i) -> (r, i)

    let add x y = x + y
    let mult x y = x * y
    let sub x y = x - y
    let div x y = x / y

    let (|+|) (c1 : complex) (c2 : complex) =
        let (a, b) = complexToPair c1
        let (c, d) = complexToPair c2
        mkComplex (add a c) (add b d) 

    let (|*|) (c1 : complex) (c2 : complex) =
        let (a, b) = complexToPair c1
        let (c, d) = complexToPair c2
        mkComplex (mult a c) (mult b d)
    
    let (|-|) (c1 : complex) (c2 : complex) =
        let (a, b) = complexToPair c1
        (|+|) c2 (mkComplex (-a) (-b))

    exception DivisionByZero

    let (|/|) (c1 : complex) (c2 : complex) =
        let (a, b) = complexToPair c2
        match (a, b) with
        | (a, b) when a = 0.0 || b = 0.0 -> raise DivisionByZero
        | _ -> (|*|) c1 (mkComplex (div a (add (mult a a) (mult b b))) (div -b (add (mult a a) (mult b b))))

    // Exercise 5
    let explode1 (s : string) = s.ToCharArray() |> List.ofArray

    let rec explode2 (s : string) = 
        match s with
        | "" -> []
        | _ -> s.Chars(0) :: explode2(s.Remove(0,1))

    // Exercise 6
    let implode (cs : char list) = List.foldBack(fun c acc -> acc + c) cs ""

    // Exercise 7

    // Exercise 8

    // Exercise 9

    // Exercise 10

    // Exercise 11

    // Exercise 12

    // Exercise 13

    // Exercise 14

    // Exercise 15

