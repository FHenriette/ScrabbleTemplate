module Gaddag
    open System.Collections.Generic
    type Dict =
        | D of Dictionary<string, string>

    let empty () : Dict = D (new Dictionary<string, string>())
    
    let insert (s : string) (d : Dict) : Dict = failwith "not implemented"
    
    let step (c : char) (d : Dict) : (bool * Dict) option = failwith "not implemented"

    let reverse (d : Dict) : (bool * Dict) option = failwith "not implemented"
   