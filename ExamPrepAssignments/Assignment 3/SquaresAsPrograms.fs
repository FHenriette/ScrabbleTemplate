module Scrabble_1
    open ImperativeLanguage

    let stmnt2SquareFun (stm : stmnt) : squareFun = 
     fun w pos acc -> 
      let s = Map.ofList [("_pos_", pos); ("_acc_", acc)]
      let res = evalStmnt stm w s
      Map.find ("_result_") res
      
    let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))
    
    let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))
    
    let containsNumbers : squareFun = 
     stmnt2SquareFun 
      (Seq (Ass ("_result_", V "_acc_"),
         While (V "i" .<. WL,
          ITE (IsDigit (CV (V "i")),
            Seq (Ass ("_result_", V "_result_" .*. N -1),
              Ass ("i", WL)),
            Ass ("i", V "i" .+. N 1)))))
   
   