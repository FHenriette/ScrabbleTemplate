// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Assignment_4
    module Program =
        let readLines filePath = System.IO.File.ReadLines filePath
        let fromFile path = 
            let ed = Trie.empty ()
            Seq.foldBack Trie.insert (readLines path) ed

        let path = @"C:\Repos\FSharp\ExamPrepAssignments\ExamPrepAssignments\Assignment 4\EnglishDictionary_test.txt"
        // let dict = fromFile path
        let queen = "TONIGHT IM GONNA HAVE MYSELF A REAL GOOD TIME I FEEL ALIVE AND THE WORLD ILL TURN IT INSIDE OUT YEAH IM FLOATING AROUND IN ECSTASY SO DONT STOP ME NOW CAUSE IM HAVING A GOOD TIME"
        let ss = readLines path |> Seq.toList
        let dict = List.fold (fun acc s -> Trie.insert s acc) (Trie.empty ()) ss
        let reverse (s : string) = Seq.rev s |> Seq.toArray |> System.String
        Array.iter (fun s -> printf "Lookup %s\t\t%b\n" s (Trie.lookup s dict)) (queen.Split ' ')

        List.iter (fun s -> if Trie.lookup (reverse s) dict then printfn "%s" s else ()) ss

        [<EntryPoint>]
        let main argv =
            printfn "%A" (Trie.empty () |> Trie.insert "HELLO");
            printfn "F %A" (Trie.lookup "HELLO" (Trie.empty ()))
            printfn "T %A" (Trie.lookup "HELLO" (Trie.empty () |> Trie.insert "HELLO"))
            printfn "F %A" (Trie.lookup "HE" (Trie.empty () |> Trie.insert "HELLO"))


            printfn "F %A" (Trie.lookup "GONNA" dict)
            printfn "F %A" (Trie.lookup "DONT" dict)

            printfn "Checking Queen"
   
    
            printfn "\n\nMary Poppins says: F %A" (Trie.lookup "SUPERCALIFRAGALISTICEXPIALIDOCIOUS" dict)
            printfn "F %A" (Trie.lookup "AB" dict)

            printfn ""
            0 // return an integer exit code
