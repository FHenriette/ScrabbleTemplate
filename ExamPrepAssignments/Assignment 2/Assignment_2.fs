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
    let implode (cs : char list) = List.foldBack(fun c acc -> (string c) + acc) cs ""
    
    let implodeRev (cs : char list) = List.fold(fun acc lst -> (string lst) + acc) "" cs

    // Exercise 7
    let toUpper s = s |> explode1 |>  List.map System.Char.ToUpper |> implode

    // Exercise 8
    let rec ack (m, n) =
        match (m, n) with
        | (0, n) -> n + 1
        | (m, 0) -> ack (m-1, 1)
        | (m, n) -> ack (m-1, ack(m, n-1))

    // Exercise 9
    let time f =
        let start = System.DateTime.Now
        let res = f ()
        let finish = System.DateTime.Now
        (res, finish - start)
            
    (*
    time (fun () -> ack (3, 11));;
    val it : int * System.TimeSpan =
        (16381, 00:00:02.6444015 {
            Days = 0;
            Hours = 0;
            Milliseconds = 644;
            Minutes = 0;
            Seconds = 2;
            Ticks = 26444015L;
            TotalDays = 3.060649884e-05;
            TotalHours = 0.0007345559722;
            TotalMilliseconds = 2644.4015;
            TotalMinutes = 0.04407335833;
            TotalSeconds = 2.6444015;})
    *)
        
    let timeArg1 f a = f a |> time 

    // Exercise 10
    let rec downto3 f n e =
        match n with
        | 0 -> e
        | n ->  downto3 f (n-1) (f n e)

    let fac n = downto3 (*) n 1

    let range g n = 
        let f x y = (g x) :: y 
        downto3 f n []

    type word = (char * int) list
    type squareFun = word -> int -> int -> int
        

    // Exercise 11
    let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)] : word

    // Exercise 12
    let singleLetterScore (word : word) pos acc = snd(word.[pos]) + acc
    let doubleLetterScore (word : word) pos acc = snd(word.[pos]) * 2 + acc
    let tripleLetterScore (word : word) pos acc = snd(word.[pos]) * 3 + acc

    // Exercise 13
    let doubleWordScore (word : word) pos acc = acc * 2
    let tripleWordScore (word : word) pos acc = acc * 3

    // Exercise 14
    let containsNumbers (word : word) pos acc = if List.exists System.Char.IsNumber (List.map fst (word)) then -acc else acc

    // Exercise 15

