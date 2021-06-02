open System
open Aug_2020.Binary_Search_Trees
open Aug_2020.Big_Intergers

let testQ1() =
    (* Testsfor Q1.1 *)

    //let t1 = insert 5 Leaf
    //let t2 = insert 3 t1
    //let t3 = insert 4 t2
    //let t4 = insert 10 t3

    //printfn "%A" t1
    //printfn "%A" t2
    //printfn "%A" t3
    //printfn "%A" t4

    ()

let testQ2() =
    // place debug prints for Q2 here
    ()

let testQ3() =
    let t0 = "" |> fromString |> toString
    let t1 = "1" |> fromString |> toString
    let t2 = "120" |> fromString |> toString
    let t3 ="12345689123456789" |> fromString |> toString

    let t4 = add (fromString "15") (fromString "39") |> toString
    let t5 = add (fromString "9995") (fromString "8") |> toString // 10003
    let t6 = add (fromString "123456789123456789") (fromString "987654321987654321") |> toString

    printfn "%A" t0
    printfn "%A" t1
    printfn "%A" t2
    printfn "%A" t3

    printfn "%A" t4
    printfn "%A" t5
    printfn "%A" t6
    ()

let testQ4 =
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    // testQ1 ()
    testQ3 ()
    0 // return an integer exit code
