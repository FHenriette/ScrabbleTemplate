// Learn more about F# at http://fsharp.org

open System
open May_2019.Peano

[<EntryPoint>]
let main argv =
    printfn "%A" (toInt (add (fromInt 2u) (fromInt 5u)))
    printfn "%A" (toInt (mult (fromInt 2u) (fromInt 5u)))
    printfn "%A" (toInt (pow (fromInt 2u) (fromInt 5u)))
    0 // return an integer exit code
