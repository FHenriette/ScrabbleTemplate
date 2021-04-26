module ImperativeLanguage
    open System
    
    type word = (char * int) list
    type squareFun = word -> int -> int -> int
    
    type aExp =
     | N of int
     | V of string
     | WL
     | PV of aExp
     | Add of aExp * aExp
     | Sub of aExp * aExp
     | Mul of aExp * aExp
    
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    
    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
    
    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"
    
    let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)] : word
    
    let rec arithEval (a : aExp) (w : word) s = 
     match a with
     | N a -> a
     | V a -> 
      let res = Map.tryFind a s
      if res.IsNone then 0 else res.Value
     | WL -> w.Length 
     | PV x -> snd (w.Item (arithEval x w s))
     | Add (a, b) -> (arithEval a w s) + (arithEval b w s)
     | Sub (a, b) -> (arithEval a w s) - (arithEval b w s)
     | Mul (a, b) -> (arithEval a w s) * (arithEval b w s)        
    
    type cExp =
       | C  of char      (* Character value *)
       | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
       | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
       | CV of aExp      (* Character lookup at word index *)
    
    let rec charEval (c : cExp) (w : word) s = 
     match c with
     | C c -> c
     | ToUpper c -> Char.ToUpper (charEval c w s)
     | ToLower c -> Char.ToLower (charEval c w s)
     | CV x -> fst (w.Item (arithEval x w s))
    
    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)
    
       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)
    
       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)
    
       | IsLetter of cExp     (* check for letter *)
       | IsDigit  of cExp     (* check for constant *)
    
    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
    
    
    let rec boolEval (b : bExp) (w : word) s = 
     match b with
     | TT -> true
     | FF -> false
     | AEq (b1, b2) -> (arithEval b1 w s) = (arithEval b2 w s)
     | ALt (b1, b2) -> (arithEval b1 w s) < (arithEval b2 w s)
     | Not b1 -> not (boolEval b1 w s)
     | Conj (b1, b2) -> (boolEval b1 w s) && (boolEval b2 w s)
     | IsLetter c -> Char.IsLetter (charEval c w s)
     | IsDigit c -> Char.IsDigit (charEval c w s)
    
    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)
    
    let rec evalStmnt (stm : stmnt) (w : word) s = 
     match stm with
     | Skip -> s
     | Ass (x, a) -> 
      let v = arithEval a w s
      Map.add x v s
     | Seq (stm1, stm2) -> 
      let s1 = evalStmnt stm1 w s
      evalStmnt stm2 w s1
     | ITE (guard, stm1, stm2) ->
      let b = boolEval guard w s
      if b then evalStmnt stm1 w s else evalStmnt stm2 w s
     | While (guard, stm) ->
      let b = boolEval guard w s
      if b then 
       let s1 = evalStmnt stm w s 
       evalStmnt (While (guard, stm)) w s1    
      else s