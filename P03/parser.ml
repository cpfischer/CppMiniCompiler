type token =
  | KEYWORD
  | WRITE
  | LPAREN
  | RPAREN
  | ASSIGN
  | IDENTIFIER of (string)
  | NEWLINE
  | NUM of (string)
  | STRVAL of (string)
  | REAL
  | INTEGER
  | STRING
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | POWER

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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
# 133 "parser.ml"
let yytransl_const = [|
  257 (* KEYWORD *);
  258 (* WRITE *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* ASSIGN *);
  263 (* NEWLINE *);
  266 (* REAL *);
  267 (* INTEGER *);
  268 (* STRING *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* MULTIPLY *);
  272 (* DIVIDE *);
  273 (* POWER *);
    0|]

let yytransl_block = [|
  262 (* IDENTIFIER *);
  264 (* NUM *);
  265 (* STRVAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\002\000\001\000\001\000\001\000\003\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\002\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\003\000\005\000\
\006\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\010\000\009\000\011\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\003\000\014\000\015\000"

let yysindex = "\007\000\
\000\000\000\000\042\255\055\255\055\255\005\255\000\000\000\000\
\000\000\007\255\009\255\011\255\055\255\000\000\095\255\018\255\
\026\255\055\255\000\000\000\000\000\000\001\255\000\000\055\255\
\055\255\055\255\055\255\055\255\000\000\018\255\021\255\021\255\
\001\255\001\255\001\255"

let yyrindex = "\000\000\
\000\000\000\000\046\000\000\000\000\000\012\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\254\
\000\000\000\000\000\000\000\000\000\000\064\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\255\254\254\100\255\
\068\255\081\255\085\255"

let yygindex = "\000\000\
\000\000\000\000\252\255"

let yytablesize = 114
let yytable = "\016\000\
\017\000\013\000\012\000\008\000\013\000\012\000\008\000\001\000\
\022\000\018\000\013\000\013\000\019\000\030\000\020\000\007\000\
\021\000\028\000\007\000\031\000\032\000\033\000\034\000\035\000\
\007\000\007\000\007\000\007\000\007\000\029\000\024\000\025\000\
\026\000\027\000\028\000\026\000\027\000\028\000\024\000\025\000\
\026\000\027\000\028\000\004\000\005\000\020\000\000\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\000\000\013\000\
\004\000\005\000\000\000\000\000\006\000\000\000\008\000\009\000\
\010\000\011\000\012\000\017\000\013\000\000\000\017\000\015\000\
\000\000\000\000\015\000\000\000\017\000\017\000\017\000\017\000\
\015\000\015\000\015\000\015\000\016\000\000\000\000\000\016\000\
\018\000\000\000\000\000\018\000\000\000\016\000\016\000\016\000\
\016\000\018\000\018\000\018\000\018\000\023\000\000\000\014\000\
\000\000\000\000\014\000\024\000\025\000\026\000\027\000\028\000\
\014\000\014\000"

let yycheck = "\004\000\
\005\000\004\001\004\001\004\001\007\001\007\001\007\001\001\000\
\013\000\005\001\013\001\014\001\006\001\018\000\006\001\004\001\
\006\001\017\001\007\001\024\000\025\000\026\000\027\000\028\000\
\013\001\014\001\015\001\016\001\017\001\004\001\013\001\014\001\
\015\001\016\001\017\001\015\001\016\001\017\001\013\001\014\001\
\015\001\016\001\017\001\002\001\003\001\000\000\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\002\001\003\001\255\255\255\255\006\001\255\255\008\001\009\001\
\010\001\011\001\012\001\004\001\014\001\255\255\007\001\004\001\
\255\255\255\255\007\001\255\255\013\001\014\001\015\001\016\001\
\013\001\014\001\015\001\016\001\004\001\255\255\255\255\007\001\
\004\001\255\255\255\255\007\001\255\255\013\001\014\001\015\001\
\016\001\013\001\014\001\015\001\016\001\007\001\255\255\004\001\
\255\255\255\255\007\001\013\001\014\001\015\001\016\001\017\001\
\013\001\014\001"

let yynames_const = "\
  KEYWORD\000\
  WRITE\000\
  LPAREN\000\
  RPAREN\000\
  ASSIGN\000\
  NEWLINE\000\
  REAL\000\
  INTEGER\000\
  STRING\000\
  PLUS\000\
  MINUS\000\
  MULTIPLY\000\
  DIVIDE\000\
  POWER\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  NUM\000\
  STRVAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
        ( )
# 258 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 138 "parser.mly"
             ( )
# 266 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
              ( )
# 272 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 142 "parser.mly"
               ( printf ""; flush stdout )
# 279 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 147 "parser.mly"
                ( _1 )
# 286 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 148 "parser.mly"
                ( _1 )
# 293 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "parser.mly"
                (   try Hashtbl.find int_table _1
                    with Not_found -> 
                    try Hashtbl.find real_table _1 with
                    Not_found -> try Hashtbl.find string_table _1 
                    with Not_found ->
                        let start_pos = Parsing.rhs_start_pos 1 in 
                        (printf "**Error** %d: Use of undeclared identifier %s\n" start_pos.pos_lnum _1; raise Parsing.Parse_error)
                )
# 307 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 157 "parser.mly"
                        ( try Hashtbl.find string_table _1; if (is_string _3) then 
                                                            (Hashtbl.replace string_table _1 _3; _3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)
                        with Not_found ->
                        try Hashtbl.find int_table _1; if (is_integer _3) then 
                                                            (Hashtbl.replace int_table _1 _3; _3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)
                            with Not_found -> 
                         try Hashtbl.find real_table _1;  if (is_real _3) then 
                                                            (Hashtbl.replace real_table _1 _3; _3;)
                                                        else
                                                            let start_pos = Parsing.rhs_start_pos 3 in 
                                                            (printf "**Error** %d: The assignment of different data types\n" start_pos.pos_lnum; raise Parsing.Parse_error)                                   
                        with Not_found ->
                            let start_pos = Parsing.rhs_start_pos 3 in 
                            (printf "**Error** %d: Use of undeclared identifier %s\n" start_pos.pos_lnum _1; raise Parsing.Parse_error)
                    )
# 335 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 178 "parser.mly"
                     (  try if (Hashtbl.mem real_table _2 || Hashtbl.mem int_table _2 || Hashtbl.mem string_table _2) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace int_table _2 "0i"; _2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum _2; raise Parsing.Parse_error )
# 348 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 186 "parser.mly"
                  ( try if (Hashtbl.mem real_table _2 || Hashtbl.mem int_table _2 || Hashtbl.mem string_table _2 ) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace real_table _2 "0f"; _2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum _2; raise Parsing.Parse_error )
# 361 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 194 "parser.mly"
                    ( try if (Hashtbl.mem real_table _2 || Hashtbl.mem int_table _2 || Hashtbl.mem string_table _2) then
                                raise (ParseError "Redefine")
                            else
                                (Hashtbl.replace string_table _2 "0f"; _2)
                        with  ParseError "Redefine" -> 
                           let start_pos = Parsing.rhs_start_pos 2 in 
                            printf "**Error** %d: Redefinition of '%s'\n" start_pos.pos_lnum _2; raise Parsing.Parse_error )
# 374 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 202 "parser.mly"
                    ( if (is_num _2) then
                        (printf "%g\n" (get_num _2); "0")
                     else 
                        (printf "%s\n" _2; "S")
                    )
# 385 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 208 "parser.mly"
               ( try add _1 _3 with 
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Addition of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error)
# 396 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 213 "parser.mly"
                ( try sub _1 _3 with 
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Subtraction of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error)
# 407 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 218 "parser.mly"
                   (try mult _1 _3 with
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Multiplication of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error)
# 418 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 223 "parser.mly"
                 ( try div _1 _3 with
                | ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                    printf "**Error** %d: Division of different data types\n"
                    start_pos.pos_lnum; raise Parsing.Parse_error )
# 429 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 228 "parser.mly"
                      ( negate _2 )
# 436 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 230 "parser.mly"
                ( try power _1 _3 with
                ParseError "DiffTypes" -> let start_pos = Parsing.rhs_start_pos 3 in 
                printf "**Error** %d: Exponential of different data types\n"
                start_pos.pos_lnum; raise Parsing.Parse_error )
# 447 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 235 "parser.mly"
                    ( _2 )
# 454 "parser.ml"
               : 'exp))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 239 "parser.mly"

# 481 "parser.ml"
