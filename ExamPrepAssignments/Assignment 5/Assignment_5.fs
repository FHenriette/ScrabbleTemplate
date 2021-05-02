module Assignment_5

// Exercise 1
let sum m n = 
    let rec aux m n acc =
        match n with
        | 0 -> m
        | _ -> aux m (n - 1) (acc + m + n)
    aux m n 0

// Exercise 2
let length lst =
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> aux xs (acc + 1)
    aux lst 0

// Exercise 3
let rec foldBackTest folder lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> folder x (foldBackTest folder xs acc)


let foldBack f lst acc =
    let rec aux c lst =
        match lst with
        | [] -> c acc
        | x :: xs -> 
            let newC res = c (f x res)
            aux newC xs 
    aux id lst

// Exercise 4
let factC =
    let rec aux c =
        function 
        | 0 -> c 1 
        | x -> aux (fun res -> c( x * res)) (x - 1) 
    aux id 

// Exercise 5
let fibA x =
    let rec aux acc1 acc2 x =
        match x with 
        | 0 -> acc1
        | _ -> let temp = (acc2 + acc1)
               aux acc2 temp (x-1)
    aux 0 1 x
    
   
let fibC x =
    let rec aux x c =
        match x with
        | 0 -> c 0
        | 1 | 2 -> c 1 
        | x -> aux (x-1) (fun y -> aux(x-2) (fun z -> c(y+z)))
    aux x id 

// Exercise 6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

// Exercise 7
type state = Map<string, int>
type word = (char * int) list

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | CharToInt of cExp     (* NEW: Cast to integer *)

and cExp =
   | C  of char             (* Character literal *)
   | CV of aExp             (* Character lookup at word index *)
   | ToUpper of cExp        (* Convert character to upper case *)
   | ToLower of cExp        (* Convert character to lower case *)
   | IntToChar of aExp      (* NEW: Cast to character *)

let rec arithEvalSimple (a:aExp) (w:word) (s:state) = 
        match a with
        | N x -> x
        | V k -> let r = Map.tryFind k s 
                 match r with
                 | Some x -> x
                 | None -> 0 
        | WL -> w.Length
        | PV x -> snd w.[(arithEvalSimple x w s)]
        | Add (x, y) -> (arithEvalSimple x w s) + (arithEvalSimple y w s)
        | Sub (x, y) -> (arithEvalSimple x w s) - (arithEvalSimple y w s)
        | Mul (x, y) -> (arithEvalSimple x w s) * (arithEvalSimple y w s)
        | CharToInt x -> (int) (charEval x w s)

and charEval (c:cExp) (w:word) (s:state) =
        match c with
        | C chr -> chr
        | CV a -> fst w.[(arithEvalSimple a w s)]
        | ToUpper x -> System.Char.ToUpper (charEval x w s)
        | ToLower x -> System.Char.ToLower (charEval x w s)
        | IntToChar x -> (char) (arithEvalSimple x w s)


// Exercise 8