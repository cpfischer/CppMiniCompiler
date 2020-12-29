(*File: token.mml*)
{
  open Parser
  open Lexing
  exception ReadError of string;;

  let inc_lineno lexbuf = 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with 
    pos_lnum = pos.pos_lnum + 1;
    }
}


let digit = ['0'-'9']
let lower_char = ['a'-'z']
let upper_char = ['A' - 'Z']
let char = ' ' | '!' | ['#' - '~']

let id = lower_char+ (lower_char | digit | '_')*
let uppercaseError = (upper_char)* id (upper_char)*
let number = digit+ 
let float_number = digit+ ('.' digit+)?
let floatError = digit+ '.'
let string = '\"'(char)*'\"' 

let keyword = 
"PROGRAM"
|"BEGIN" 
| "END"

let write = "WRITE"


rule token = parse
| [' ' '\t']                { token lexbuf}
| keyword                  { token lexbuf }
| '\n'                      { inc_lineno lexbuf; NEWLINE }
| number as intnum          { NUM (intnum^"i") }
| float_number as num       { NUM (num^"r") }
| string as str             { STRVAL (str) }
| ":="                      { ASSIGN } 
| "INTEGER"                 { INTEGER }
| "REAL"                    { REAL }
| "STRING"                  { STRING }
| id as identifier          { IDENTIFIER (identifier) (*if (String.length identifier) <= 25 then
                                let _ = IDENTIFIER (identifier)
                              else
                                let _ = Printf.printf("*Error* Identifier length exceeds 25 characters") in token lexbuf 
                            *)}      
(* | uppercaseError            { Printf.printf("Error: no uppercase chars in id\n") } *)
| ';'                       { token lexbuf }
| write                     { WRITE }
| '+'                       { PLUS }
| '-'                       { MINUS }
| '*'                       { MULTIPLY }
| '/'                       { DIVIDE }
| "**"                      { POWER }
| '('                       { LPAREN }
| ')'                       { RPAREN }
(* | string as s               { STRING s 
                            ; token lexbuf
                            } *)
| _                         { token lexbuf }
| eof                       { raise End_of_file }
