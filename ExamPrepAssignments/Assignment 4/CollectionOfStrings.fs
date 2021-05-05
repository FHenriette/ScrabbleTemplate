module CollectionOfStrings
    type Dict = 
        | D of string List

    let empty () = D (List.empty)
    let insert (s : string) (D d) = D (s :: d)
    let lookup (s : string) (D d) = List.contains s d

   