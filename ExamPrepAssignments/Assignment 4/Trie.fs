module Trie
    type Dict =
        | D of Map<char, bool * Dict>

    let empty () : Dict = D Map.empty
    
    let step (c : char) (D m) : (bool * Dict) option = Map.tryFind c m

    let rec insert (s : string) (D m as d) : Dict = 
        if s = "" then d else 
            match Map.tryFind s.[0] m with
            | None -> D (Map.add (s.[0]) (s.Length = 1, insert (s.Remove(0, 1))  d) m)
            | Some (b, d1) -> D (Map.add (s.[0]) (b || s.Length = 1, insert (s.Remove(0, 1)) d1) m)

    let rec lookup (s : string) (D m as d) : bool = 
        if s = "" then false else 
            match step (s.[0]) d with
            | None -> false
            | Some(b, d1) -> if b && s.Length = 1 then true else lookup (s.Remove(0, 1)) d1