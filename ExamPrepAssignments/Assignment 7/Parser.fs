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
    
    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (a, b) -> (a :: b) |> System.String.Concat
    
    let unop op a : Parser<'b> = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let cParse, cref = createParserForwardedToRef<cExp>()
    
    let bTermParse, btref = createParserForwardedToRef<bExp>()
    let bProdParse, bpref = createParserForwardedToRef<bExp>()
    let bAtomParse, baref = createParserForwardedToRef<bExp>()

    let sTermParse, stref = createParserForwardedToRef<stm>()
    let sAtomParse, saref = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse] <?> "string"

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul        <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div        <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod        <?> "Mod"
    let CharToInt = pCharToInt >*>. parenthesise cParse |>> CharToInt   <?> "CharToInt"
    do pref := choice [MulParse; DivParse; ModParse; CharToInt; AtomParse] <?> "string"

    let NegParse = pchar '-' >>. pint32 |>> (fun a -> Mul (N -1, N a))
    let PointValueParse = pPointValue >*>. parenthesise TermParse |>> PV <?> "PointValue"
    let VariableParse = pid |>> V <?> "Var"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [ NegParse; PointValueParse; VariableParse; NParse; ParParse ] <?> "string"

    let AexpParse = TermParse 

    let cToLower = pstring "toLower" >*>. parenthesise cParse |>> ToLower           <?> "ToLower"
    let cToUpper = pstring "toUpper" >*>. parenthesise cParse |>> ToUpper           <?> "ToUpper"
    let cIntToChar = pIntToChar >*>. parenthesise TermParse |>> IntToChar           <?> "IntToChar"
    let cChar = pchar '\'' >>. (pletter <|> whitespaceChar) .>> pchar '\'' |>> C    <?> "Char"
    let cCharValue = pstring "charValue" >*>. parenthesise TermParse |>> CV         <?> "CharValue"
    do cref := choice [ cCharValue; cChar; cIntToChar; cToUpper; cToLower ]         <?> "string"

    let CexpParse = cParse

    let bConj = binop (pstring @"/\") bProdParse bTermParse |>> (fun (a, b) -> a .&&. b) <?> "Conjunction"
    let bDisj = binop (pstring @"\/") bProdParse bTermParse |>> (fun (a, b) -> a .||. b) <?> "Disjunction"
    do btref := choice [ bConj; bDisj; bProdParse ] <?> "string"

    let bEqual = binop (pchar '=') TermParse TermParse |>> (fun (a, b) -> a .=. b)                              <?> "Equality"
    let bNEqual = binop (pstring "<>") TermParse TermParse |>> (fun (a, b) -> a .<>. b)                         <?> "Inequality"
    let bLessThan = binop (pchar '<') TermParse TermParse |>> (fun (a, b) -> a .<. b)                           <?> "Less than"
    let bLessThanEqual = binop (pstring "<=") TermParse TermParse |>> (fun (a, b) -> a .<=. b)                  <?> "Less than equal"
    let bGreaterThan = binop (pchar '>') TermParse TermParse |>> (fun (a, b) -> a .>. b)                        <?> "Greater than"
    let bGreaterThanEqual = binop (pstring ">=") TermParse TermParse |>> (fun (a, b) -> a .>=. b)               <?> "Greater than equal"
    do bpref := choice [ bEqual; bNEqual; bLessThan; bLessThanEqual; bGreaterThan; bLessThanEqual; bAtomParse ] <?> "string"

    let bTrue = pstring "true" |>> fun _ -> TT
    let bFalse = pstring "false" |>> fun _ -> FF
    let bParens = parenthesise bTermParse
    let bNot = unop (pchar '~') bAtomParse |>> (fun a -> (~~) a) <?> "Not"
    let bIsLetter = cParse |>> IsLetter
    let bIsDigit = cParse |>> IsDigit
    do baref := choice [ bNot; bIsLetter; bIsDigit; bTrue; bFalse; bParens ] <?> "string"

    let BexpParse = bTermParse

    let sSemi = binop (pchar ';') sAtomParse sTermParse |>> Seq
    do stref := choice [sSemi; sAtomParse] <?> "string"

    let sif = pstring "if" >*>. (parenthesise bTermParse .>*> pstring "then" .>*>. ( brackets sTermParse) ) |>> fun (a,b) -> ITE (a, b, Skip)
    let sife = pstring "if" >*>. (parenthesise bTermParse .>*> pstring "then" .>*>. ( brackets sTermParse .>*>. (pstring "else" >*>. brackets sTermParse)) ) |>> fun (a, (b,c)) -> ITE (a, b, c)
    let sWhile = pstring "while" >*>. (parenthesise bTermParse .>*>. (pstring "do" >*>. (brackets sTermParse))) |>> While
    let sVariable = pid .>*>. (pstring ":=" >*>. TermParse) |>> Ass <?> "Assignment"
    let sDeclare = pstring "declare"  |>> Declare
    do saref := choice [sWhile; sife; sif; sVariable; sDeclare] <?> "string"

    let stmntParse = sTermParse

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

    let parseSquareFun (sqp: squareProg) : square = Map.map (fun _ s w -> fun pos acc -> stmntToSquareFun (run stmntParse s |> getSuccess) w pos acc) sqp

    let parseBoardFun (s : string) (m : Map<int, 'a>) = stmntToBoardFun (run stmntParse s |> getSuccess) m

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) : board =
        let m' : Map<int, square> = Map.map (fun _ v -> parseSquareFun v) bp.squares

        {
            center = bp.center;
            defaultSquare = m'.[bp.usedSquare]
            squares = parseBoardFun bp.prog m'
        }
