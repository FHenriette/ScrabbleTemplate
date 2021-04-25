module Assignment_1

    // Exercise 1
    let sqr x = x * x

    // Exercise 2
    let pow x y = System.Math.Pow(x, y)

    // Exercise 3
    let rec sum n =
        match n with
        | 0 -> n
        | _ -> n + (sum n)

    // Exercise 4
    let rec fib n =
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ -> fib(n-1) + fib(n-2)

    // Exercise 5
    let rec fact =
        function
        | 0 -> 1
        | n -> n * fact(n-1)

    let rec power =
        function
        | (x,0) -> 1.0
        | (x,n) -> x * power(x,n-1)

    // Determine a type for each of the expressions:
    // (System.Math.PI, fact -1)        --> float * int
    // fact(fact 4)                     --> int
    // power(System.Math.PI, fact 2)    --> float
    // (power, fact)                    --> fun * fun

    // Exercise 6
    let dup s = s + s

    // Exercise 7
    let rec dupn s n = 
        match n with
        | 0 -> s
        | _ -> s + dupn s (n-1)

    // Exercise 8
    let rec bin (n, k) =
        match k with
        | 0 -> 1
        | k when k = n -> 1
        | _ -> bin (n-1, k-1) + bin (n-1, k)

    // Exercise 9
    let rec f =
        function
        | (0,y) -> y
        | (x,y) -> f(x-1, x*y)

    // 1. Determine the type of f.                                  --> int * int -> int
    // 2. For which arguments does the evaluation of f terminate?   --> When x is 0
    // 3. Write the evaluation steps for f(2, 3).
        // f(x-1, x*y) --> f(2-1, 2*3)
        // f(1, 6)
        // f(x-1, x*y) --> f(1-1, 1*6)
        // f(0, 6)
        // y           --> 6
    // 4. What is the mathematical meaning of f(x, y)?              --> Factorial

    // Exercise 10
    // Consider the following declaration:
    
    let test(c,e) = if c then e else 0

    // 1. What is the type of test?                                                 --> bool * int -> int
    // 2. What is the result of evaluating test(false, fact(−1)) ?                  --> StackOcerflowException
    // 3. Compare this with the result of evaluating if false then fact -1 else 0   --> It evaluates before checking the bool in the if-statement

    // Exercise 11


    // Exercise 12

    // Exercise 13

    // Exercise 14

    // Exercise 15