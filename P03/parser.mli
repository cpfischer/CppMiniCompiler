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

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
