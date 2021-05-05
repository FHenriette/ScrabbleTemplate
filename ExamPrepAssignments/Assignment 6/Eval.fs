﻿namespace Assignment_6

module Eval =
    open StateMonad
    open System

    (* Code for testing *)

    let hello = [(* INSERT YOUR DEFINITON OF HELLO HERE.*)] 
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
        | Add (a1, a2) -> binop (+) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop (-) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop (*) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> divisor (%) (arithEval a1) (arithEval a2)
        | CharToInt c  -> (charEval c >>= fun a -> ret (int a))
    
    and charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | CV x -> (arithEval x >>= fun a -> characterValue a)
        | ToLower x -> (charEval x >>= fun a -> ret (Char.ToLower(a)))
        | ToUpper x -> (charEval x >>= fun a -> ret (Char.ToUpper(a)))
        | IntToChar x -> (arithEval x >>= fun a -> ret (char a))

    let boolEval b : SM<bool> = 
        let rec aux = function
        | TT -> ret true
        | FF -> ret false
        | AEq (x,y) -> (binop (=) (arithEval x) (arithEval y))
        | ALt (x,y) -> (binop (<) (arithEval x) (arithEval y))
        | Not bx -> (unop (not) (aux bx))
        | Conj (bx, by) -> binop (&&) (aux bx) (aux by)
        | IsLetter cx -> (charEval cx >>= fun a -> ret (Char.IsLetter(a)))
        | IsDigit cx -> (charEval cx >>= fun a -> ret (Char.IsDigit(a)))
        aux b


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
        | While (bExp1, stm1) -> boolEval bExp1 >>= fun t -> push >>>= (if t then stmntEval stm1 >>>= stmntEval stmnt else ret ()) >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let rec arithEval2 a : SM<int> =
        match a with
        | N n -> prog.Return n
        | V x -> lookup x
        | PV x -> (arithEval x >>= fun a -> pointValue a)
        | WL -> wordLength
        | Add (a1, a2) -> binop (+) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop (-) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop (*) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> divisor (%) (arithEval a1) (arithEval a2)
        | CharToInt c  -> (charEval c >>= fun a -> ret (int a))


    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    