%{
open Printf
open Lexing
let int_table = Hashtbl.create 100;;
let real_table = Hashtbl.create 100;;
let string_table = Hashtbl.create 100;;


let get_num x = let n = (String.length x) in 
    let num = String.sub x 0 (n - 1) in float_of_string num;;

 
let is_string x = (String.contains x '\"');;

let is_integer x = ((String.contains x 'i') && not(is_string x));;

let is_real x = ((String.contains x 'r') && not (is_string x));;

let is_num x = (is_integer x) || (is_real x);;

let is_valid_op x y = (((is_integer x) && (is_integer y)) || ((is_real x) && (is_real y)));;




exception ParseError of string;;

let add x y = if (is_num x && is_num y) then
                if is_valid_op x y then 
                    if ((is_integer x) && (is_integer y)) then
                        (string_of_float (get_num x +. get_num y))^"i"
                    else
                        (string_of_float (get_num x +. get_num y))^"r"
                else
                    raise (ParseError "DiffTypes")
              else      
                    let start_pos = Parsing.rhs_start_pos 3 in 
                    (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
            ;;

let sub x y = if (is_num x && is_num y) then
                if is_valid_op x y then 
                    if (is_integer x && is_integer y) then
                        (string_of_float (get_num x -. get_num y))^"i"
                    else
                        (string_of_float (get_num x -. get_num y))^"r"
                else
                    raise (ParseError "DiffTypes")
              else      
                    let start_pos = Parsing.rhs_start_pos 3 in 
                    (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error);;

let mult x y = if (is_num x && is_num y) then
                if is_valid_op x y then 
                    if (is_integer x && is_integer y) then
                        (string_of_float (get_num x *. get_num y))^"i"
                    else
                        (string_of_float (get_num x *. get_num y))^"r"
                else
                    raise (ParseError "DiffTypes")
               else      
                    let start_pos = Parsing.rhs_start_pos 3 in 
                    (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
            ;;

let div x y = if (is_num x && is_num y) then
                if is_valid_op x y then 
                    if (is_integer x && is_integer y) then
                        if (get_num y) <> 0.0 then 
                            (string_of_float (get_num x /. get_num y))^"i"
                        else 
                            let start_pos = Parsing.rhs_start_pos 3 in 
                            printf "**Error** %d: division by zero\n"
                            start_pos.pos_lnum; raise Parsing.Parse_error
                        
                    else
                        if (get_num y) <> 0.0 then 
                            (string_of_float (get_num x /. get_num y))^"r"
                        else    
                            let start_pos = Parsing.rhs_start_pos 3 in 
                            printf "**Error** %d: division by zero\n"
                            start_pos.pos_lnum; raise Parsing.Parse_error
                else
                    raise (ParseError "DiffTypes")
             else      
                let start_pos = Parsing.rhs_start_pos 3 in 
                (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error);;                                   
            
let negate x = if (is_num x) then
                    if (is_integer x) then
                        (string_of_float (-.get_num x))^"i"
                    else
                        (string_of_float (-.get_num x))^"r"
                else      
                    let start_pos = Parsing.rhs_start_pos 3 in 
                    (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
            ;;

let power x y = if (is_num x && is_num y) then
                    if is_valid_op x y then
                    if (is_integer x && is_integer y) then
                        (string_of_float (get_num x ** get_num y))^"i"
                    else
                        (string_of_float (get_num x ** get_num y))^"r"
                    else
                        raise (ParseError "DiffTypes")
                else      
                    let start_pos = Parsing.rhs_start_pos 3 in 
                    (printf "**Error** %d: No string operations\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
            ;;
%}

/*TOKENS*/
%token KEYWORD
%token WRITE
%token LPAREN RPAREN ASSIGN
%token <string> IDENTIFIER
%token NEWLINE
%token <string> NUM
%token <string> STRVAL

%token REAL
%token INTEGER
%token STRING 

%token PLUS MINUS MULTIPLY DIVIDE
%token NEWLINE
%token POWER

%left PLUS MINUS 
%left MULTIPLY DIVIDE
%left NEG
%right POWER
%start input
%type <unit> input
%% /*GRAMMAR*/
input:  { }
| input line { }
;

line: NEWLINE { }
| exp NEWLINE  { printf ""; flush stdout }
;


exp:
| NUM           { $1 }
| STRVAL        { $1 }
| IDENTIFIER    {   try Hashtbl.find int_table $1
                    with Not_found -> 
                    try Hashtbl.find real_table $1 with
                    Not_found -> try Hashtbl.find string_table $1 
                    with Not_found ->
                        let start_pos = Parsing.rhs_start_pos 1 in 
                        (printf "**Error** %d: Use of undeclared identifier %s\n" start_pos.pos_lnum $1; raise Parsing.Parse_error)
                }
| IDENTIFIER ASSIGN exp { try Hashtbl.find string_table $1; if (is_string $3) then 
                                                            (Hashtbl.replace string_table $1 $3; $3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)
                        with Not_found ->
                        try Hashtbl.find int_table $1; if (is_integer $3) then 
                                                            (Hashtbl.replace int_table $1 $3; $3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)
                            with Not_found -> 
                         try Hashtbl.find real_table $1;  if (is_real $3) then 
                                                            (Hashtbl.replace real_table $1 $3; $3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
                        with Not_found ->
                            let start_pos = Parsing.rhs_start_pos 3 in 
                            (printf "**Error** %d: Use of undeclared identifier %s\n" start_pos.pos_lnum $1; raise Parsing.Parse_error)
                    }
| INTEGER IDENTIFIER {  try if (Hashtbl.mem real_table $2 || Hashtbl.mem int_table $2 || Hashtbl.mem string_table $2) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace int_table $2 "0i"; $2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum $2; raise Parsing.Parse_error }

| REAL IDENTIFIER { try if (Hashtbl.mem real_table $2 || Hashtbl.mem int_table $2 || Hashtbl.mem string_table $2 ) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace real_table $2 "0f"; $2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum $2; raise Parsing.Parse_error }

| STRING IDENTIFIER { try if (Hashtbl.mem real_table $2 || Hashtbl.mem int_table $2 || Hashtbl.mem string_table $2) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace string_table $2 "0f"; $2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum $2; raise Parsing.Parse_error }

| WRITE exp         { if (is_num $2) then
                        (printf "%g\n" (get_num $2); "0")
                     else 
                        (printf "%s\n" $2; "S")
                    }

| exp PLUS exp { try add $1 $3 with 
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Addition of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error}

| exp MINUS exp { try sub $1 $3 with 
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Subtraction of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error}

| exp MULTIPLY exp {try mult $1 $3 with
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Multiplication of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error}

| exp DIVIDE exp { try div $1 $3 with
                | ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                    printf "**Error** %d: Division of different data types\n"
                    start_pos.pos_lnum; raise Parsing.Parse_error }

| MINUS exp %prec NEG { negate $2 }

| exp POWER exp { try power $1 $3 with
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Exponential of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error }

| LPAREN exp RPAREN { $2 }
;
/*printf "%0.0g\n"*/
%%

