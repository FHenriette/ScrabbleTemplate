// Insert your MultiSet.fs file here. All modules must be internal
namespace Scrabble
module internal MultiSet =
    type 'a MultiSet when 'a : comparison =
        | MS of Map<'a, uint32>
        override this.ToString() = 
            match this with
            | MS this -> 
                let lst = Map.foldBack (fun key value state -> List.append [(sprintf "(%A, #%A)" key (int value))] state) this []
                "{" + String.concat ", " lst + "}"

    let empty = MS Map.empty 

    let isEmpty (MS ms) = Map.isEmpty ms

    let size (MS ms) = Map.foldBack(fun _ acc v -> v + acc) ms 0u 

    let contains key (MS ms) = Map.containsKey key ms

    let numItems a (MS ms) = 
        match Map.tryFind a ms with
        | None -> 0u
        | Some(x) -> x

    let add a (n:uint32) (MS ms) = 
        match Map.tryFind a ms with
        | None -> MS(Map.add a n ms)
        | Some(prev) -> MS(Map.add a (n + prev) ms) 

    let addSingle a ms = add a 1u ms

    let remove a (n:uint32) (MS ms) = 
        match Map.tryFind a ms with 
        | None -> MS ms
        | Some(prev) when prev > n -> MS(Map.add a (prev-n) ms)
        | _ -> MS (Map.remove a ms)

    let removeSingle a ms = remove a 1u ms

    let fold f acc (MS ms) = Map.fold f acc ms 

    let foldBack f (MS ms) acc = Map.foldBack f ms acc  

    let ofList lst = List.foldBack (fun x acc -> addSingle x acc) (lst) (empty)

    let toList (MS ms) = Map.fold (fun lst k (v:uint32) -> lst @ List.replicate (int v) k) [] ms

    let map f ms = toList (ms) |> List.map f |> ofList 

    let union ms1 (MS ms2) = Map.fold (fun acc k v -> add k 0u acc) ms1 ms2

    let sum ms1 (MS ms2) = Map.fold (fun acc k v -> add k v acc) ms1 ms2

    let subtract ms1 (MS ms2) = Map.fold (fun acc k v -> remove k v acc) ms1 ms2

    let intersection ms1 (MS ms2) = Map.fold (fun acc k v -> if contains k acc then acc else remove k v acc) ms1 ms2
