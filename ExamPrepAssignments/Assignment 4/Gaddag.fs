module Gaddag
    type Dict =
        | D of Map<char, bool * Dict>

    let empty () : Dict = D Map.empty
    
    let step (c : char) (D m) : (bool * Dict) option = Map.tryFind c m

    let reverse (d : Dict) : (bool * Dict) option = failwith "not implemented"
        //match  with
        //| None -> insert (s.Remove(0)) (D (Map.add (s.[0]) (false, d) m))
        //| Some (b, d1) -> insert (s.Remove(0)) d1

    let rec insert (s : string) (D m as d) : Dict = 
        match Map.tryFind s.[0] m with
        | None -> insert (s.Remove(0)) (D (Map.add (s.[0]) (false, d) m))
        | Some (b, d1) -> insert (s.Remove(0)) d1