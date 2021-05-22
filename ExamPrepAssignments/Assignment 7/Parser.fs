namespace Assignment_7

open JParsec

module ImpParser =

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar" 
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charToValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = 
        let predicate = System.Char.IsWhiteSpace
        satisfy predicate

    let pletter        = 
        let predicate = System.Char.IsLetter
        satisfy predicate  

    let palphanumeric  = 
        let predicate = System.Char.IsLetterOrDigit
        satisfy predicate

    let spaces         = whitespaceChar |> many

    let spaces1        = whitespaceChar |> many1
    
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2

    let parenthesise p = 
        let start = pchar '('
        let ending = pchar ')'
        start >*>. p .>*> ending 

    let brackets p = 
        let start = pchar '{'
        let ending = pchar '}'
        start >*>. p .>*> ending 
    
    let fromCharListToString (cs : char list) = 
        let rec aux acc (xs : char list) =
            match xs with
            | [] -> pstring acc
            | y::ys -> aux (acc + string y) ys
        aux "" cs

    (*
    Using the parsers pletter , palphanumeric , and many from Assignment 7.2, along with the
    combinators (|>>) and .>>. (not .>*>. as we do not want spaces in our identifiers), and
    pchar , create a parser pid : Parser<string> that parses identifiers that start with a single
    letter or underscore followed by an arbitrary number of letters, numbers, or underscores.
    *)
    
    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (_, _) -> fromCharListToString
    
    let unop op a = failwith "not implemented"
    let binop _ p1 p2 = p1 .>>. p2 // incorrect (not implemented)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) = failwith "not implemented"

