module Trie
    type Dict =
        | D of Map<char, bool * Dict>

    let empty () : Dict = D Map.empty

    let rec insert (s : string) (d : Dict) : Dict = 
        match Map.tryFind s.[0] d with
        | None -> insert (s.Remove(0)) (D (Map.add s.[0] (false, d)))
        | Some (b, d1) -> insert (s.Remove(0)) d1

    let rec lookup (s : string) (d : Dict) : bool = 
        match Map.tryFind s.[0] d with
        | None -> false
        | Some(b, d1) -> lookup (s.Remove(0)) d1

    let step (c : char) (d : Dict) : (bool * Dict) option = 
        match Map.tryFind c d with
        | None -> None
        | Some(b,d) -> Some(b,d)