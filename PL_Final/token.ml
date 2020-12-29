(*
        File: token.ml
        Name: Caleb Fischer
*)

type token = Int_tok of int
            | Bool_tok of bool
            | Float_tok of float
            | Id_tok of string
            | Plus_tok of char
            | Minus_tok of char
            | Times_tok of char
            | Div_tok of char
            | Lt_tok of char
            | Lte_tok of string
            | Gt_tok of char
            | Gte_tok of string
            | Is_Equ_tok of string
            | Is_Not_Equ_tok of string
            | Bool_Or_tok of string
            | Bool_And_tok of string
            | Equ_tok of char
            | If_tok of string
            | Else_tok of string
            | Colon_tok of char
            | Lparen_tok of char
            | Rparen_tok of char
            | Lcomment_tok of string
            | Rcomment_tok of string
            | Error_tok of string;;

exception IgnoreCase;;
let print_token i = match i with
    Int_tok i -> i
;;
(* Write a print_token function. For instance print_token (Int_tok 5)
print
Int_tok 5
on the console window.
*)
(* Write a print_tokens function. For instance print_tokens [Int_tok 5,
Float 3.1, Id_tok "num_heads", Plus_tok] prints
[Int_tok 5, Float 3.1, Id_tok "num_heads", Plus_tok]
on the console window.
*)
