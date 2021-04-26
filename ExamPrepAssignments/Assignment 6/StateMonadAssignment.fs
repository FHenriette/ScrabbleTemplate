﻿namespace Assignment_6

module StateMonadAssignment =
    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = {  vars     : Map<string, int> list
                    word     : (char * int) list 
                    reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
            { vars = [Map.ofList lst];
                word = word;
                reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
            match a s with
            | Success (av, s') ->
                let (S g) = f av
                g s'
            | Failure err -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))      

    let wordLength : SM<int> = S (fun s -> Success(List.length s.word, s))      

    let characterValue (pos : int) : SM<char> = 
        S (fun s ->
            if s.word.Length > pos && pos >= 0
            then Success(fst (List.item pos s.word), s)
            else Failure(IndexOutOfBounds pos)
            )

    let pointValue (pos : int) : SM<int> = 
        S (fun s ->
            if s.word.Length > pos && pos >= 0
            then Success(snd(List.item pos s.word), s)
            else Failure(IndexOutOfBounds pos)
            )

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
                match aux (s.vars) with
                | Some v -> Success (v, s)
                | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S (fun s ->
            match s with
            | _ when Set.contains var s.reserved        -> Failure(ReservedName var)
            | _ when Map.containsKey var s.vars.Head    -> Failure(VarExists var)
            | _ -> Success((), {s with vars = (Map.add var 0 Map.empty)::s.vars}))

    let update (var : string) (value : int) : SM<unit> = 
        let rec aux =
            function
            | []        -> None
            | m :: ms   ->
                match Map.tryFind var m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
                match aux (s.vars) with
                | Some v -> Success((), {s with vars = (Map.add var value Map.empty)::s.vars})
                | None -> Failure(VarNotFound var))

    