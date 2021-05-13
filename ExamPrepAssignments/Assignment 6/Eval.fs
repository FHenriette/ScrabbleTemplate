namespace Assignment_6

module Eval =
    open StateMonad
    open System

    (* Code for testing *)

    let hello =  [('H', 4)(**;('E', 1);('L', 1);('L', 1);('O', 1)**)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let binop f a b = a >>= fun x -> b >>= fun y -> ret(f x y)
    let add a b = binop (+) a b
    
    let divisor f a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (f x y) else fail DivisionByZero
    let div a b = divisor (/) a b   
    
    let switch f x = ret (f x)

    let unop f a = a >>= switch f

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V x -> lookup x
        | PV x -> (arithEval x >>= fun a -> pointValue a)
        | WL -> wordLength
        | Add (e1, e2) -> arithEval e1 >>= (fun e -> arithEval e2 >>= (fun r -> ret (e + r)))
        | Sub (e1, e2) -> arithEval e1 >>= (fun e -> arithEval e2 >>= (fun r -> ret (e - r)))
        | Mul (e1, e2) -> arithEval e1 >>= (fun e -> arithEval e2 >>= (fun r -> ret (e * r)))
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (e1, e2) -> arithEval e1 >>= (fun r -> arithEval e2 >>= (fun e -> if (e <> 0) then ret (r % e) else fail DivisionByZero))
        | CharToInt c  -> (charEval c >>= fun a -> ret (int a))
    
    and charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | CV x -> (arithEval x >>= characterValue)
        | ToLower x -> (charEval x >>= fun a -> ret (Char.ToLower(a)))
        | ToUpper x -> (charEval x >>= fun a -> ret (Char.ToUpper(a)))
        | IntToChar x -> (arithEval x >>= fun a -> ret (char a))

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1, a2) -> arithEval a1 >>= (fun e -> arithEval a2 >>= (fun r -> ret (e = r)))
        | ALt (a1, a2) -> arithEval a1 >>= (fun e -> arithEval a2 >>= (fun r -> ret (e < r)))
        | Not e -> boolEval e >>= (fun r -> ret (not r))
        | Conj (a1, a2) -> boolEval a1 >>= (fun e -> boolEval a2 >>= (fun r -> ret (e && r)))
        | IsLetter cx -> (charEval cx >>= fun a -> ret (Char.IsLetter(a)))
        | IsDigit cx -> (charEval cx >>= fun a -> ret (Char.IsDigit(a)))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare s -> declare s
        | Ass (s, a) -> arithEval a >>= fun x -> update s x
        | Skip -> ret () 
        | Seq (stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2    
        | ITE (bExp1, stm1, stm2) -> boolEval bExp1 >>= fun t -> push >>>= (if t then stmntEval stm1 else stmntEval stm2) >>>= pop
        | While (bExp1, stm1) -> boolEval bExp1 >>= (fun t -> if t then push >>>= stmntEval stm1 >>>= pop >>>= stmntEval (While (bExp1, stm1)) else ret ()) >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let rec arithEval2 a = 
        match a with
        | N n -> prog.Return n
        | V x -> 
            prog {
                return! (lookup x)
            } 
        | PV x -> 
            prog { 
                let! x = (arithEval2 x)
                return! pointValue x 
            }
        | WL -> wordLength
        | Add (a1, a2) ->
            prog {
                let! i1 = arithEval2 a1
                let! i2 = arithEval2 a2
                return i1 + i2
            }
        | Sub (a1, a2) -> 
            prog {
                let! i1 = arithEval2 a1
                let! i2 = arithEval2 a2
                return i1 - i2
            }
        | Mul (a1, a2) -> 
            prog {
                let! i1 = arithEval2 a1
                let! i2 = arithEval2 a2
                return i1 * i2
            }
        | Div (a1, a2) -> 
            prog {
                return! div (arithEval2 a1) (arithEval2 a2)
            }
        | Mod (a1, a2) -> 
            prog {
                return! divisor (%) (arithEval2 a1) (arithEval2 a2)
            }
        | CharToInt c  ->
            prog { 
                let! x = (charEval c)
                return (int x) 
            }
    let rec charEval2 c = 
        match c with
        | C x -> prog.Return x
        | CV x ->
            prog { 
                let! x = (arithEval2 x)
                return! characterValue x 
            }
        | ToLower x ->
            prog { 
                let! c = (charEval2 x)
                return Char.ToLower(c) 
            }
        | ToUpper x ->
            prog { 
                let! c = (charEval2 x)
                return Char.ToUpper(c) 
            }
        | IntToChar x ->
            prog { 
                let! a = (arithEval x)
                return (char a) 
            }
    let rec boolEval2 b = 
        match b with
        | TT -> prog.Return true
        | FF ->  prog.Return false
        | AEq (x,y) ->
            prog {
                return! (binop (=) (arithEval2 x) (arithEval2 y))
            }
        | ALt (x,y) ->
            prog {
                return! (binop (<) (arithEval2 x) (arithEval2 y))
            }
        | Not bx -> 
            prog {
                return! (unop (not) (boolEval2 bx))
            }
        | Conj (bx, by) ->
            prog {
                return! (binop (&&) (boolEval2 bx) (boolEval2 by))
            }
        | IsLetter cx ->
            prog { 
                let! a = (charEval2 cx)
                return Char.IsLetter(a) 
            }
        | IsDigit cx ->
            prog { 
                let! a = (charEval2 cx)
                return Char.IsDigit(a) 
            }

    let rec stmntEval2 stm = 
        match stm with
        | Declare s -> 
            prog {
                do! declare s
            }
        | Ass (s, a) ->
            prog {
                let! x = arithEval2 a
                do! update s x
            }
        | Skip -> prog.Return ()
        | Seq (stm1, stm2) ->
            prog {
                do! stmntEval2 stm1
                do! stmntEval2 stm2
            }
        | ITE (bExp1, stm1, stm2) -> 
            prog { 
                let! b = boolEval2 bExp1
                do! push
                do! (if b then stmntEval stm1 else stmntEval stm2)
                do! pop
            }
        | While (bExp1, stm1) -> 
             prog {
                let! b = boolEval2 bExp1
                do! push
                do! (
                    if b then 
                        prog {
                            do! stmntEval stm1 
                            do! stmntEval stm 
                        }
                    else prog.Return ()
                )
                do! pop
             }


(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun (stm: stm) : squareFun =  
        fun w pos acc -> 
            let initS = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_result_"]
            let look = stmntEval stm >>>= lookup "_result_"
            evalSM initS look

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let rec stmntToBoardFun stm m : boardFun = failwith "Not implemented"
         

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    