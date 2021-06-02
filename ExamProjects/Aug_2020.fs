namespace Aug_2020 
    module Binary_Search_Trees =
        type 'a bintree =
            | Leaf
            | Node of 'a bintree * 'a * 'a bintree
        // Question 1.1
        let rec insert x (t : 'a bintree) =
            match t with
            | Leaf -> Node(Leaf, x, Leaf)
            | Node (tl, a, tr) when x <= a -> Node (insert x tl, a, tr) 
            | Node (tl, a, tr) -> Node (tl, a, insert x tr)

        // let t1 = insert 5 Leaf
        // let t2 = insert 3 t1
        // let t3 = insert 4 t2
        // let t4 = insert 10 t3

        // Question 1.2
        let fromList (lst : 'a List) : 'a bintree =
            let rec aux acc ys =
                match ys with
                | [] -> acc
                | x::xs -> aux (insert x acc) xs
            aux Leaf lst

        // let testFromList = fromList [5;3;4;10]

        // Question 1.3
        let rec fold f acc t =
            match t with
            | Leaf -> acc
            | Node (Leaf, node, Leaf) -> f acc node
            | Node (tl, node, tr) -> fold f (f (fold f acc tl) node) tr
                
        // let testFold = fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10]) // Expected output = 6

        let rec foldBack f acc t = 
            match t with
            | Leaf -> acc
            | Node (Leaf, node, Leaf) -> f acc node
            | Node (tl, node, tr) -> foldBack f (f (foldBack f acc tr) node) tl

        // let testFoldBack = foldBack (fun acc x -> x - acc) 0 (fromList [3;5;4;10]) // Expected output: -6

        let inOrder t = fold (fun acc x -> x :: acc) [] t |> List.rev

        // let testInOrder = inOrder (fromList [5;3;4;10])

        //let rec badMap f =
        //    function
        //    | Leaf -> Leaf
        //    | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

        // Doesn't take into account the structure of the tree
        // badMap (fun x -> -x) (fromList [3; 5; 4; 10]);;
        // Node (Leaf, -3, Node (Node (Leaf, -4, Leaf), -5, Node (Leaf, -10, Leaf)))

        // let testBadMap = badMap (fun x -> -x) (fromList [3;5;4;10])

        let rec map f t = fold (fun acc x -> insert (f x) acc) Leaf t

        // let testMap = map (fun x -> -x) (fromList [3;5;4;10])

    module Code_Comprehension =
        let rec foo =
            function
            | [x] -> [x]
            | x::y::xs when x > y -> y :: (foo (x::xs))
            | x::xs -> x :: foo xs

        let rec bar =
            function
            | [x] -> true
            | x :: y :: xs -> x <= y && bar (y :: xs)

        let rec baz =
            function
            | [] -> []
            | lst when bar lst -> lst
            | lst -> baz (foo lst) 

        // What are the types of functions foo , bar , and baz?
        // foo : 'a list -> 'a list when 'a : comparison
        // bar : 'a list -> bool when 'a : comparison
        // baz : 'a list -> 'a list when 'a : comparison

        // What do functions bar , and baz do (not foo , we admit that it is a bit contrived)? 
        // Focus on what they do rather than how they do it.
        // Bar returns true if the list is ascending order
        // Baz returns the list ascending order

        // What would be appropriate names for functions foo , bar , and baz?
        // Bar -> isAscendingOrder
        // Baz -> ascendingOrder

        // Question 2.2
        // Both foo and bar don't take the empty list [] as a pattern match
        // Baz takes the empty list as a pattern match, and is therefore not in problems because of foo and bar
        let rec foo2 =
            function
            | [] -> []
            | [x] -> [x]
            | x::y::xs when x > y -> y :: (foo (x::xs))
            | x::xs -> x :: foo xs

        let rec bar2 =
            function
            | [] -> true
            | [x] -> true
            | x :: y :: xs -> x <= y && bar (y :: xs)

        // Question 2.3
        let rec foo3 =
            function
            | [x] -> [x]
            | x::xs -> x :: foo3 xs
            | x::y::xs when x > y -> y :: (foo3 (x::xs))

        // foo3 will never reach third case because it will always run second match because it doesn't filter.
        // let list = [2;1;3;5]
        // let testFoo = foo list
        // let testFoo3 = foo3 list

        // Question 2.4
        let bar3 =
            function
            | [] -> true
            | [x] -> true
            | x :: xs -> List.fold (fun (b,x) y -> b && x <= y, y) (true, x) xs |> fst

        // Question 2.5
        // foo is not tail recursive because it doesn't call itself as the last thing, but need to be concatenated after the last call
        let fooTail lst =
            let rec aux c =
                function
                | [] -> c []
                | [x] -> c [x]
                | x :: y :: xs when x > y -> aux (fun res -> c (y :: res)) (x::xs)
                | x :: xs -> aux (fun res -> c (x :: res)) xs
            aux id lst
    module Big_Intergers =
        // Question 3.1
        type bigInt = List<int>

        let rec fromString nums : bigInt =
            match nums with
            | "" -> List.empty
            | _ -> (int) (nums.[0].ToString()) :: fromString (nums.Remove(0,1)) 
            
        let rec toString (x : bigInt) : string =
            match x with
            | [] -> ""
            | x::xs -> x.ToString() + toString xs

        // Question 3.2
        let add (x : bigInt) (y : bigInt) : bigInt =
            let xs = List.rev x
            let ys = List.rev y
            let rec aux xs ys carry acc =
                match (xs, ys) with
                | [], [] -> if carry > 0 then carry :: acc else acc
                | x :: xs, [] -> 
                    let res = x + carry
                    let carry = res / 10
                    if carry > 0 
                    then aux xs [] 1 (res - 10 :: acc) |> List.rev
                    else ((x + carry) :: acc @ xs) |> List.rev
                | [], y :: ys -> 
                    let res = y + carry
                    let carry = res / 10
                    if carry > 0 
                    then aux [] ys 1 (res - 10 :: acc) |> List.rev
                    else ((y + carry) :: acc @ ys) |> List.rev
                | x :: xs, y :: ys -> 
                    let res = x + y + carry
                    let carry = res / 10
                    if carry > 0 
                    then aux xs ys 1 (res - 10 :: acc) |> List.rev
                    else aux xs ys 0 (res :: acc) |> List.rev
            aux xs ys 0 []

        // Question 3.3

        // Question 3.4

        // Question 3.5
        
    module Lazy_List =
        type 'a llist =
        | Cons of (unit -> ('a * 'a llist))

        let rec llzero = Cons (fun () -> (0, llzero))

        // Question 4.1
        let step ll =
            match ll with
            | Cons f -> f ()

        let cons x ll =
            Cons (fun () -> (x, ll))

        // Question 4.2            

        // Question 4.3

        // Question 4.4

        // Question 4.5

        // Question 4.6