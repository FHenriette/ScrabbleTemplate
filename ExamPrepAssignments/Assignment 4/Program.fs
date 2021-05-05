// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    //printfn "%A" (Dict.empty () |> Dict.insert "HELLO");
    //printfn "F %A" (Dict.lookup "HELLO" (Dict.empty ()))
    //printfn "T %A" (Dict.lookup "HELLO" (Dict.empty () |> Dict.insert "HELLO"))
    //printfn "F %A" (Dict.lookup "HE" (Dict.empty () |> Dict.insert "HELLO"))

    let readLines filePath = System.IO.File.ReadLines filePath
    //let fromFile path = 
    //    let ed = Dict.empty ()
    //    Seq.foldBack Dict.insert (readLines path) ed
    
    let path = @"C:\Repos\FSharp\ExamPrepAssignments\ExamPrepAssignments\Assignment 4\EnglishDictionary_test.txt"
    //let dict = fromFile path
    //let queen = "TONIGHT IM GONNA HAVE MYSELF A REAL GOOD TIME I FEEL ALIVE AND THE WORLD ILL TURN IT INSIDE OUT YEAH IM FLOATING AROUND IN ECSTASY SO DONT STOP ME NOW CAUSE IM HAVING A GOOD TIME"
    //printfn "F %A" (Dict.lookup "GONNA" dict)
    //printfn "F %A" (Dict.lookup "DONT" dict)

    //printfn "Checking Queen"
    //Array.iter (fun s -> printf "Lookup %s\t\t%b\n" s (Dict.lookup s dict)) (queen.Split ' ')
    
    //printfn "\n\nMary Poppins says: F %A" (Dict.lookup "SUPERCALIFRAGALISTICEXPIALIDOCIOUS" dict)

    let ss = readLines path |> Seq.toList
    let dict = List.fold (fun acc s -> Trie.insert s acc) (Trie.empty ()) ss
    let reverse (s : string) = Seq.rev s |> Seq.toArray |> System.String
    List.iter (fun s -> if Trie.lookup (reverse s) dict then printfn "%s" s else ()) ss

    printfn "F %A" (Trie.lookup "AB" dict)

    printfn ""
    0 // return an integer exit code
