(*File: main.ml*)
(*Author: Caleb Fischer*)
(*Note: no indentation support, some single line nested if/else support*)

#use "lexer.mml.ml";;

(*
let rec getOp list = match list with
  | [] -> []
  | x::xs -> if x != Int_tok || x != Float_tok then
               x
             else
               getOp xs;;
 *)
(*Bool "true" and Bool "false"*)

let checkType x = match x with
  | Int_tok x -> 1
  | Float_tok x -> 0
  | _ -> 2
;;

let isNum x = match x with
  | Int_tok x -> true
  | Float_tok x -> true
  | Bool_tok x -> true
  | _ -> false
;;

let isPlusMin x = match x with
  | Plus_tok x -> true
  | Minus_tok x -> true
  | _ -> false
;;


let isMin x = match x with
  | Minus_tok x -> true
  | _ -> false
;;


let isBool x = match x with
  | Bool_tok x -> true
  | _ -> false
;;

let isMulDiv x = match x with
  | Times_tok x -> true
  | Div_tok x -> true
  | _ -> false
;;

let isBoolOp x = match x with
  | Lt_tok x -> true
  | Lte_tok x -> true
  | Gt_tok x -> true
  | Gte_tok x -> true
  | Is_Equ_tok x -> true
  | Is_Not_Equ_tok x -> true
  | Bool_Or_tok x -> true
  | Bool_And_tok x -> true
  | Equ_tok x -> true
  | _ -> false
;;

let isLparen x = match x with
  | Lparen_tok x -> true
  | _ -> false
;;

let isRparen x = match x with
  | Rparen_tok x -> true
  | _ -> false
;;

let getInt x = match x with
  | Int_tok x -> x
  | Bool_tok x -> if x then
                    1
                  else
                    0
;;

let isId x = match x with
  | Id_tok x -> true
  | _ -> false
;;

let getFloat x = match x with
  | Float_tok x -> x
  | Int_tok x -> float_of_int x
  | Bool_tok x -> if x then
                    float_of_int 1
                  else
                    float_of_int 0
;;

let doIntOp x y z = match y with
  | Plus_tok y -> getInt x + getInt z
  | Minus_tok y -> getInt x - getInt z
  | Times_tok y -> getInt x * getInt z
  | Div_tok y -> getInt x / getInt z
;;

let doFloatOp x y z = match y with
  | Plus_tok y -> getFloat x +. getFloat z
  | Minus_tok y -> getFloat x -. getFloat z
  | Times_tok y -> getFloat x *. getFloat z
  | Div_tok y -> getFloat x /. getFloat z
;;

let checkNegative x = match x with
  | Minus_tok x -> 1
  | _ -> 0
;;


(*These two work*)
let negativeFloat x = match x with
    Float_tok x -> Float_tok (x *. -1.0)
;;


let negativeInt x = match x with
    Int_tok x -> Int_tok (x * -1)
;;

(*let doCalc list = match list with
    x::y::z::xs ->
    if (checkType x) = 1 && (checkType z) = 1 then
      if isBoolOp y then
        print_int(doIntOp x y z)
      else
        print_bool()
    else
      print_float(doFloatOp x y z)
;;
 *)

(*reverse list for popops check*)
let rec rev list = match list with
    [] -> []
  | x::xs -> (rev xs) @ [x]
;;

(*Pop if new operator is higher precedence or equal to previous top of stack*)
let rec isPopOps ops topMulDiv topPlusMin topBoolOp = match ops with
    [] -> false
  | y::ys -> if (isMulDiv y) && (topMulDiv = false && topPlusMin = false && topBoolOp = false) then
               isPopOps ys true false  false
             else if (isPlusMin y) && (topMulDiv = false && topPlusMin = false && topBoolOp = false) then
               isPopOps ys false true false
             else if (isBoolOp y) && (topMulDiv = false && topPlusMin = false && topBoolOp = false) then
               isPopOps ys false false true
             else if (isBoolOp y) && (topBoolOp) then
               true
             else if (isPlusMin y) && (topPlusMin || topBoolOp) then (*new op was a mul/div or plus/min or bool*)
               true
             else if (isMulDiv y) && (topMulDiv || topPlusMin || topBoolOp) then (*new op was a mul/div on top of mul/div or bool*)
               true
             else
               false
;;


let popOps ops = match ops with
  | [] -> []
  | x::xs -> xs
;;

let popHead ops = match ops with
  | [] -> []
  | x::xs -> [x]
;;

let popHeadVal list = match list with
    [] -> Error_tok "popHeadVal error"
  | x::xs -> x
;;

let convertNeg x = match x with
  | Int_tok x -> Int_tok (x * -1)
  | Float_tok x -> Float_tok (x *. -1.0)
;;


let convertBool x = match x with
  | Bool_tok x -> if x = true then
                    Int_tok 1
                  else
                    Int_tok 0
;;

let rec getIdVal x symTable isReturn = match symTable with
  | [] -> [Error_tok "idNotFound"]
  | y::ys -> if isReturn then
               [y]
             else if y = x then
               getIdVal x ys true (*search for string*)
             else
               getIdVal x ys false
;;

(*Checks if id is in symTable*)
let rec idExists x symTable = match symTable with
  | [] -> false
  | y::ys -> if y = x then
               true
             else
               idExists x ys
;;

let rec popParenOutput output ops = match ops with
    [] -> output
  | x::xs -> if (isLparen x) then
               output
             else
               popParenOutput (output @ [x]) xs
;;

let rec popParenOps ops = match ops with
    [] -> []
   | x::xs -> if (isLparen x) then
               xs
             else
               popParenOps xs
;;

let rec shuntingYard list output ops symTable prevNeg = match list with
    [] -> (output @ ops) (*Reverse instead of popping one by one*)
  | x::xs -> if (isRparen x) then
               shuntingYard xs (popParenOutput output ops) (popParenOps ops) symTable false
             else if (isId x) then (*Convert id to its value and pass that in instead*)
               if (idExists x symTable) then
                 shuntingYard ((getIdVal x symTable false) @ xs) output ops symTable prevNeg
               else
                 shuntingYard xs output ops ([x] @ symTable) false
             else if (isNum x) then (*Number tokens*)
               if prevNeg then
                 shuntingYard xs (output @ [convertNeg x]) ops symTable false (*number * -1 or *. -1.0*)
               else
                 shuntingYard xs (output @ [x]) ops symTable false
             else if (isBool x) then (*Bool token*)
               shuntingYard ([convertBool x] @ xs) output ops symTable false
             else if (isMulDiv x) then (* times div tokens *)
               if (isPopOps ([x] @ ops) false false false) then
                   shuntingYard xs (output @ (popHead ops)) ([x] @ (popOps ops)) symTable false
               else
                 shuntingYard xs output ([x] @ ops) symTable false
             else if (isPlusMin x) then (* Plus Minus tokens*)
               if (isMin x) && (prevNeg = false) && List.length output = 0 then (*First number is negative*)
                 shuntingYard xs output ops symTable true
               else if (isMin x) && (prevNeg = false) then
                 if (isPopOps ([Plus_tok '+'] @ ops) false false false) then (*replace minus with a plus token to operator stack*)
                   shuntingYard xs (output @ (popHead ops)) ([Plus_tok '+'] @ (popOps ops)) symTable true
                 else
                   shuntingYard xs output (([Plus_tok '+'] @ ops)) symTable true
               else if (isMin x) && prevNeg then (*Throw away second minus in 1 - -2 case*)
                 shuntingYard xs output ops symTable false
               else (*Should be a plus token*)
                 if (isPopOps ([x] @ ops) false false false) then
                   shuntingYard xs (output @ (popHead ops)) ([x] @ (popOps ops)) symTable false
                 else
                   shuntingYard xs output ([x] @ ops) symTable false
             else if (isLparen x) then
               shuntingYard xs output ([x] @ ops) symTable false
             else if (isBoolOp x) then (* bool operators *)
               if (isPopOps ([x] @ ops) false false false) then
                   shuntingYard xs (output @ (popHead ops)) ([x] @ (popOps ops)) symTable false
               else
                 shuntingYard xs output ([x] @ ops) symTable false
             else
               [Error_tok "shuntingYardError"]
;;

let rec split list = match list with
    []->[]
  (*negative unary*)
  (*if function like 1 - 2 then returns (1)(-2), isOp asks if op before current*)
  | x::xs -> if checkType x = 1 then
               x::(split xs)
             (*float*)
             else if checkType x = 0 then
               x::(split xs)
             (*is operator*)
             else
               x::(split xs)
;;

let makeList () = let list = read_line() in (split (tokenize list))
;;


let isInt x = match x with
  | Int_tok x -> 1
  | _ -> 0
;;


let doAdd numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Int_tok (getInt x + (getInt y))
                else
                  Float_tok (getFloat x +. (getFloat y))
;;

let doMinus numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Int_tok (getInt x - (getInt y))
                else
                  Float_tok (getFloat x -. (getFloat y))
;;

let doTimes numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Int_tok (getInt x * (getInt y))
                else
                  Float_tok (getFloat x *. (getFloat y))
;;

let doDiv numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Int_tok (getInt x / (getInt y))
                else
                  Float_tok (getFloat x /. (getFloat y))
;;

let doLt numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x < (getInt y))
                else
                  Bool_tok (getFloat x < (getFloat y))
;;

let doLte numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x <= (getInt y))
                else
                  Bool_tok (getFloat x <= (getFloat y))
;;

let doGt numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x > (getInt y))
                else
                  Bool_tok (getFloat x > (getFloat y))
;;

let doGte numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x >= (getInt y))
                else
                  Bool_tok (getFloat x >= (getFloat y))
;;

let doIsEq numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x == (getInt y))
                else
                  Bool_tok (getFloat x == (getFloat y))
;;

let doIsNotEq numbers = match numbers with
  | x::y::xs -> if isInt x = 1 && isInt y = 1 then
                  Bool_tok (getInt x != (getInt y))
                else
                  Bool_tok (getFloat x != (getFloat y))
;;

let getBool x = match x with
  | Int_tok x -> if x = 1 then
                   true
                 else
                   false
  | Bool_tok x -> if x = true then
                    true
                  else
                    false
  | Float_tok x -> if x = 1.0 then
                     true
                   else
                     false
;;

let doOr numbers = match numbers with
  | x::y::xs -> Bool_tok (getBool x || (getBool y))           
;;

let doAnd numbers = match numbers with
  | x::y::xs -> Bool_tok (getBool x && (getBool y))
;;

let opSwitch x = match x with
  | Plus_tok x -> 1
  | Minus_tok x -> 2
  | Times_tok x -> 3
  | Div_tok x -> 4
  | Lt_tok x -> 5
  | Lte_tok x -> 6
  | Gt_tok x -> 7
  | Gte_tok x -> 8
  | Is_Equ_tok x -> 9
  | Is_Not_Equ_tok x -> 10
  | Bool_Or_tok x -> 11
  | Bool_And_tok x -> 12
  | _ -> 0
;;

let doCalc operator numbers = match operator with
  | x::xs -> if opSwitch x = 1 then
               doAdd numbers
             else if opSwitch x = 2 then
               doMinus numbers
             else if opSwitch x = 3 then
               doTimes numbers
             else if opSwitch x = 4 then
               doDiv numbers
             else if opSwitch x = 5 then
               doLt numbers
             else if opSwitch x = 6 then
               doLte numbers
             else if opSwitch x = 7 then
               doGt numbers
             else if opSwitch x = 8 then
               doGte numbers
             else if opSwitch x = 9 then
               doIsEq numbers
             else if opSwitch x = 10 then
               doIsNotEq numbers
             else if opSwitch x = 11 then
               doOr numbers
             else if opSwitch x = 12 then
               doAnd numbers
             else
               Error_tok "e"
;;

let isOperator x = isPlusMin x || isMulDiv x || isBoolOp x
;;


let rec doOps list leftList operator numbers testBool = match list with
  | x::xs -> if (List.length xs) = 0 && (List.length numbers) = 0 && (List.length operator) = 0 || testBool then
               x::xs
             else if isNum x then
               if List.length operator = 1 && List.length numbers = 1 then
                 doOps (leftList @ ([doCalc operator ([x] @ numbers)]) @ xs) [] [] [] false (*case: operator exists and 1 number*)
               else
                 doOps xs leftList operator ([x] @ numbers) false (*Case: numbers and/or operator empty*)
             else if isOperator x then
               if List.length operator = 0 then
                 doOps xs leftList [x] numbers false (*Case: no operator yet*)
               else if List.length operator = 1 then
                 doOps xs (leftList @ operator @ numbers) [x] [] false (*Case: operator in array, pop it and numbers to the leftList and replace*)
               else
                 [Error_tok "doOps error"]
             else
               [Error_tok "doOps error"]
;;


let popExpr list = match list with
  | x::xs -> x
;;

let convertBool x = if x = true then
                      [Bool_tok true]
                    else
                      [Bool_tok false]
;;

(*converts expression to [Bool_tok x]*)
(*NEED FUNCTION TO CONVERT EXPRESSIONS TO INT BOOL OR FLOAT*)
let doExpr expr1 expr2 op = match op with
  | Lt_tok op -> convertBool (expr1 < expr2)
  | Lte_tok op -> convertBool (expr1 <= expr2)
  | Gt_tok op -> convertBool (expr1 > expr2)
  | Gte_tok op -> convertBool (expr1 >= expr2)
  | Is_Equ_tok op -> convertBool (expr1 == expr2)
  | Is_Not_Equ_tok op -> convertBool (expr1 != expr2)
  | Bool_Or_tok op -> convertBool (expr1 || expr2)
  | Bool_And_tok op -> convertBool (expr1 && expr2)
;; 


(*let rec makeExpr list expr1 expr2 op = match list with
  |  [] -> if (List.length expr2) > 0 then
             doExpr (popExpr expr1) (popExpr expr2) op
           else
             expr1
  | x::xs -> if isBoolOp x then
               makeExpr xs (doOps(rev(shuntingYard expr1 [] [] false)) [] [] [] false) [] [x]
             else if List.length op = 0 then
               makeExpr xs (expr1 @ [x]) [] []
             else
               makeExpr xs expr1 (expr2 @ [x]) op
;;
 *)

let isEquTok x = match x with
  | Equ_tok x -> true
  | _ -> false
;;

let rec isAssign list symTable isVar isEqu = match list with
    [] -> if isVar || isEqu = false then false
          else true
  | x::xs -> if isVar && isEqu then
               true
             else if isId x then
               isAssign xs symTable true false
             else if isEquTok x then
               isAssign xs symTable isVar true
             else
               isAssign xs symTable false false
;;

let print_bool x = if x then
                     print_string "true"
                   else
                     print_string "false"
;;

let rec print x = match x with
  | Int_tok x -> print_int x
  | Float_tok x -> print_float x
  | Bool_tok x -> print_bool x
;;

let print_val list = match list with
    [] -> print_string "[]\n"
  | x::xs -> let _ = print x in print_string "\n>> "
;;

let print_symbol x returnVal = match x with
    Id_tok x -> let _ = print_string x in
                let _ = print_string "=" in
                print_val returnVal
;;

(*Do math*)
let getReturnVal list symTable = (doOps(rev(shuntingYard (list) [] [] symTable false)) [] [] [] false)
;;

(*Adds id token and value of expr to symTable uses format x = 3 + 4*)
let getNewSymVal list symTable = match list with
  | x::y::xs -> getReturnVal xs symTable
;;
(*MAKE SYMTABLE UPDATE THROUGHOUT WHOLE LOOP
 Function for Update and docalc there or something else*)

let updateSymTable list returnVal symTable = match list with
  | [] -> []
  | x::y::xs -> ([x] @ returnVal @ symTable)
;;

let printNewSymVal x returnVal = match x with
  | Id_tok x -> let _ = print_string x in
             let _ = print_string " = " in
             print_val returnVal
;;

let isIfTok x = match x with
  | If_tok x -> true
  | _ -> false
;;

let isElseTok x = match x with
  | Else_tok x -> true
  | _ -> false
;;

let isIf list = match list with
    [] -> false
  | x::xs -> if isIfTok x then
               true
             else
               false
;;

let isColonTok x = match x with
  | Colon_tok x -> true
  | _ -> false
;;

let rec getIfCon list ifCon = match list with
  | [] -> [Error_tok "Error in getIfCon"]
  | x::xs -> if isColonTok x = false then
               getIfCon xs (ifCon @ [x])
             else
               ifCon
;;

let doIfCon list symTable = match list with
  | [] -> false
  | x::xs -> if (getBool (popHeadVal (getReturnVal (getIfCon xs []) symTable))) then
               true
             else
               false
;;

let rec getIfStmt list ifStmt isIfStmt pastCol nestCount = match list with
  | [] -> ifStmt
  | x::xs -> if isIfTok x && isIfStmt = false then
               getIfStmt xs [] true pastCol nestCount
             else if isIfTok x && isIfStmt then
               let nestCount = nestCount + 1 in getIfStmt xs (ifStmt @ [x]) true pastCol nestCount
             else if isColonTok x && isIfStmt && (pastCol = false) then
               getIfStmt xs [] isIfStmt true nestCount
             else if isElseTok x && nestCount = 0 then
               ifStmt
             else if isElseTok x && nestCount > 0 then
               let nestCount = nestCount - 1 in getIfStmt xs (ifStmt @ [x]) true true nestCount
             else if pastCol && isIfStmt then
               getIfStmt xs (ifStmt @ [x]) true true nestCount
             else
               getIfStmt xs [] isIfStmt pastCol nestCount
;;

let rec getElseStmt list elseStmt isElse pastCol = match list with
  | [] -> elseStmt
  | x::xs -> if isElseTok x && isElse = false then
               getElseStmt xs [] true pastCol
             else if isElseTok x && isElse then
               elseStmt
             else if isColonTok x && isElse then
               getElseStmt xs [] true true
             else if isElse && pastCol then
               getElseStmt xs (elseStmt @ [x]) isElse pastCol
             else
               getElseStmt xs elseStmt isElse pastCol
;;

(*Need to loop and return symtable only every time, have prints with returnVal*)

let doStmt list symTable  =
  if List.length list = 0 then
    symTable
  else if (isAssign list symTable false false) then
    let returnVal = getNewSymVal list symTable in
    let _ = (printNewSymVal (popHeadVal list) returnVal) in (updateSymTable list returnVal symTable)                                                      
  else
    let _ = let returnVal = getReturnVal list symTable in print_val returnVal in symTable
;;


let rec loop list symTable = if List.length list = 0 then
                               let list = makeList() in loop list symTable
                             else if (isIf list) then
                               if (doIfCon list symTable) then
                                 (*getIfStmt list [] false false 0*)
                                 loop (getIfStmt list [] false false 0) symTable
                               else (*if was false, do else*)
                                 if List.length (getElseStmt list [] false false) = 0 then
                                   let _ = print_string ">> " in loop [] symTable
                                 else
                                   loop (getElseStmt list [] false false) symTable
                             else
                               loop [] (doStmt list symTable)
;;

(* INDENTATION HANDLER
let rec checkSpaces list spaceCount = match list with
  | [] -> spaceCount
  | x::xs -> if x = " " then
               let spaceCount = spaceCount + 1 in checkSpaces xs spaceCount
             else
               spaceCount
;;

let rec stripSpaces list = match list with
  | [] -> []
  | x::xs -> if x = ' ' then
               stripSpaces xs
             else
               xs
;;

let evalIfElse ifBlock elseBlock symTable =
  if (doIfCon (getIfCon ifBlock []) symTable) then
    doStmt (getIfStmt ifBlock [] false false) symTable
  else
    doStmt (getElseStmt elseBlock false) symTable
;;
 *)
(* BROKEN CAN'T HANDLE INDENTATION
let rec loop list symTable isIfBlock isElseBlock ifBlock elseBlock =
  if isIfBlock then
    let list = makeList() in if (List.length list = 0) then (*Needs to return symTable*)
                               loop [] (evalIfElse ifBlock elseBlock symTable) false false [] []
                             else
                               if (checkSpaces list 0 = 4) then
                                 loop [] symTable true false (ifBlock @ (stripSpaces list)) elseBlock
                               else if (checkSpaces list 0 = 0) then
                                 loop list symTable false false ifBlock elseBlock
                               else
                                 [Error_tok "Indentation error"]
  else if List.length list = 0 then
    let list = makeList() in loop list symTable isIfBlock isElseBlock ifBlock elseBock
  else if (isIf list) then (*add first line and block data to block*)
      loop [] symTable true false (ifBlock @ [list])
  else
    loop [] (doStmt list symTable)
;;
 *)
(*List.length list*)
let main () = let _ = print_string ">> " in loop [] []
;;

main();;
(*
let main = let list = tokenize(s) in doOp list;;
 *)
