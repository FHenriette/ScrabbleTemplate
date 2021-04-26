module ArithExp
    // Exercise 1
    type aExp =
        | N of int
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
    
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)

    let a1 = N 42
    let a2 = N 4 .+. (N 5 .-. N 6)
    let a3 = N 4 .*. N 2 .+. N 34
    let a4 = (N 4 .+. N 2) .*. N 34
    let a5 = N 4 .+. (N 2 .*. N 34) 
    
    let rec arithEvalSimple (a : aExp) = 
        match a with
        | N a -> a
        | Add (a, b) -> (arithEvalSimple a) + (arithEvalSimple b)
        | Sub (a, b) -> (arithEvalSimple a) - (arithEvalSimple b)
        | Mul (a, b) -> (arithEvalSimple a) * (arithEvalSimple b)
   