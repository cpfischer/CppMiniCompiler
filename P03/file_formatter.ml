let rec single_line list = match list with
        [] -> []
    | x::tail -> if x == '\n' then
                    x::tail
                 else
                    single_line tail;;

let rec multi_line list = match list with 
        [] -> []
    | x::y::tail -> if x == '*' && y == '/' then
                        tail
                    else 
                        multi_line (y::tail)
    | _::[] -> [];;


let get_after_comment list = match list with
    | [] -> []
    | x::tail -> if x == '/' then
                    single_line tail
                else if x == '*' then
                    multi_line tail
                else 
                    x::tail;;

let rec get_after_string list = match list with
    [] -> []
    | x::tail -> if x == '\"' then
                    tail
                else
                    get_after_string tail;;

let rec add_string output list = match list with
    [] -> output
    | x::tail -> if x == '\"' then
                    (output @ [x])
                else
                    add_string (output @ [x]) tail;;

let check_comment x y = x == '/' && (y =='/' || y == '*');;

let check_string x = x == '\"';;

let rec remove_comments output lines = match lines with
        x::[] -> output @ [x]
    | [] -> output
    | x::y::tail -> if check_string x then
                    remove_comments (add_string (output @ [x]) (y::tail)) (get_after_string (y::tail))
                else if check_comment x y then
                    remove_comments output (get_after_comment (y::tail))
                else
                    remove_comments (output @ [x]) (y::tail)
    ;;

let rec remove_successive lines = match lines with
        [] -> []
    | x::tail -> if x == ' ' || x == '\t' then
                    remove_successive tail
                else
                    x::tail;;

let rec remove_whitespace output lines = match lines with 
        [] -> output
    | x::tail -> if check_string x then
                    remove_whitespace (add_string (output @ [x]) tail) (get_after_string tail) 
                else if x == ' ' || x == '\t' then
                    remove_whitespace (output @ [' ']) (remove_successive tail)
                 else
                    remove_whitespace (output @ [x]) tail;;
    

let read_entire_file filename =
    let pfile = open_in filename in
    let mycpp = really_input_string pfile (in_channel_length pfile) in 
    close_in pfile;
    mycpp;;

let list_of_string s =
    let rec list n =
        if n = String.length s then []
        else s.[n]::list (n + 1) in
    list 0;;

let list_to_string chars = 
    let buffer = Buffer.create 16 in
    List.iter (Buffer.add_char buffer) chars;
    Buffer.contents buffer;;

let print_file source_f filename = let pfile = open_out(filename^".bac") in
    output_string pfile("This is my target program: \n"^source_f);
    close_out pfile;;

exception NotMyCpp of string;;

let is_mycpp filename = Filename.check_suffix filename ".mycpp";;
let file_formatter = let filename = Sys.argv.(1) in 
    if (is_mycpp filename) then
        list_to_string(remove_whitespace [] (remove_comments [] (list_of_string(read_entire_file (filename)))))
    else
     raise (NotMyCpp "Not .mycpp file");;

let filename = (Filename.chop_suffix (Sys.argv.(1)) ".mycpp")