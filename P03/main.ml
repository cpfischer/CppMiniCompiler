(* let read_entire_file filename =
    let pfile = open_in filename in
    let mycpp = really_input_string pfile (in_channel_length pfile) in 
    close_in pfile;
    mycpp;;

let main = let mycpp = 
    let filename = (Filename.chop_suffix (Sys.argv.(1)) ".mycpp") in 
    read_entire_file filename in
    Parser.input Lexer.token Lexing.mycpp
;;

main;; *)
open Printf
let main () = 
try 
let lexbuf = Lexing.from_channel stdin in
while true do
Parser.input Lexer.token lexbuf
done
with End_of_file -> printf "\n--- No syntax errors ---\n"; flush stdout 
let _ = main ()
