// Learn more about F# at http://fsharp.org

open System
open Aug_2020.Binary_Search_Trees

[<EntryPoint>]
let main argv =
    printfn "%A" (insert 5 Leaf)
    printfn "%A" (insert 3 t1)
    printfn "%A" (insert 4 t2)
    printfn "%A" (insert 10 t3)

    printfn "%A" (fromList [5;3;4;10])

    printfn "%A" (fold (fun acc x -> x - acc) 0 (fromList [3;5;4;10])) // Expected output = 6

    printfn "%A" (foldBack (fun acc x -> x - acc) 0 (fromList [3;5;4;10])) // Expected output: -6
    0 // return an integer exit code
