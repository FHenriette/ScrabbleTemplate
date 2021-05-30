namespace Aug_2020 
    module Binary_Search_Trees =
        type 'a bintree =
            | Leaf
            | Node of 'a bintree * 'a * 'a bintree
        // Question 1.1
        let rec insert x (t : 'a bintree) =
            match t with
            | Leaf -> Node(Leaf, x, Leaf)
            | Node (tl, a, tr) when x < a -> Node (insert x tl, a, tr) 
            | Node (tl, a, tr) when x > a -> Node (tl, a, insert x tr)
            | _                           -> t

        let t1 = insert 5 Leaf
        let t2 = insert 3 t1
        let t3 = insert 4 t2
        let t4 = insert 10 t3

        // Question 1.2
        let fromList (lst : 'a List) : 'a bintree =
            let rec aux acc ys =
                match ys with
                | [] -> acc
                | x::xs -> aux (insert x acc) xs
            aux Leaf lst

        let testFromList = fromList [5;3;4;10]

        // Question 1.3
        let rec fold f acc t =
            match t with
            | Leaf -> acc
            | Node (Leaf, node, Leaf) -> f acc node
            | Node (tl, node, Leaf) -> fold f (f acc node) tl
            | Node (tl, _, tr) -> fold f (fold f acc tl) tr
                
        let testFold = fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10]) // Expected output = 6

        let rec foldBack f acc t = 
            match t with
            | Leaf -> acc
            | Node (Leaf, node, Leaf) -> f acc node
            | Node (Leaf, node, tr) -> foldBack f (f acc node) tr
            | Node (tl, _, tr) -> foldBack f (foldBack f acc tr) tl

        let testFoldBack = foldBack (fun acc x -> x - acc) 0 (fromList [3;5;4;10]) // Expected output: -6

        let inOrder t = fold (fun acc x -> x :: acc) [] t |> List.rev

        let testInOrder = inOrder (fromList [5;3;4;10])

        let rec badMap f =
            function
            | Leaf -> Leaf
            | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

        // Provide example why this i a bad mapping function

        let rec map f t = fold (fun x -> f x) t

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


        // What would be appropriate names for functions foo , bar , and baz?