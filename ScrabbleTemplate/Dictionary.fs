// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

    type Dict = 
        | D of string List

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty () = D (List.empty)
    let insert (s : string) (D d) = D (s :: d)
    let lookup (s : string) (D d) = List.contains s d
    let step : char -> Dict -> (bool * Dict) option = fun _ _ -> failwith "Not implemented"


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"