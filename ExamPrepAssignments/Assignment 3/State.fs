module State
    // Exercise 2
    type aExp =
        | N of int
        | V of string
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
    
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    
    
    type state = Map<string, int>

    let a6 = V "x"
    let a7 = N 4 .+. (V "y" .-. V "z")
    
    let rec arithEvalState (a : aExp) (s : state) = 
        match a with
        | N a -> a
        | V a -> 
            let res = Map.tryFind a s
            if res.IsNone then 0 else res.Value
        | Add (a, b) -> (arithEvalState a s) + (arithEvalState b s)
        | Sub (a, b) -> (arithEvalState a s) - (arithEvalState b s)
        | Mul (a, b) -> (arithEvalState a s) * (arithEvalState b s)        

   