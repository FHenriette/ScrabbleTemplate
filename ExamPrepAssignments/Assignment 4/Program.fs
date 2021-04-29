// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    //printfn "%A" (Dict.empty () |> Dict.insert "HELLO");
    printfn "F %A" (Dict.lookup "HELLO" (Dict.empty ()))
    printfn "T %A" (Dict.lookup "HELLO" (Dict.empty () |> Dict.insert "HELLO"))
    printfn "F %A" (Dict.lookup "HE" (Dict.empty () |> Dict.insert "HELLO"))

    let readLines filePath = System.IO.File.ReadLines filePath
    let fromFile path = 
        let ed = Dict.empty ()
        Seq.foldBack Dict.insert (readLines path) ed
    let dict = fromFile @"C:\Repos\FSharp\ExamPrepAssignments\ExamPrepAssignments\Assignment 4\EnglishDictionary.txt"
    let queen = "TONIGHT IM GONNA HAVE MYSELF A REAL GOOD TIME I FEEL ALIVE AND THE WORLD ILL TURN IT INSIDE OUT YEAH IM FLOATING AROUND IN ECSTASY SO DONT STOP ME NOW CAUSE IM HAVING A GOOD TIME"
    
    printfn "Checking Queen"
    Array.iter (fun s -> printf "Lookup %s\t\t%b\n" s (Dict.lookup s dict)) (queen.Split ' ')
    
    printfn "\n\nMary Poppins says: %A" (Dict.lookup "SUPERCALIFRAGALISTICEXPIALIDOCIOUS" dict)

    printfn ""
    0 // return an integer exit code
