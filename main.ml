
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;
let read_input ()=
  let mark () = print_string ">> " in
  let prompt () = print_string "   " in
  mark();
  let rec aux ls = 
    let line = read_line() in 
    match (String.rindex_opt line ';') with
      Some index ->
        if (try line.[index-1] = ';' with _ -> false)
          then String.concat " " (List.rev ((String.sub line 0 (index - 1))::ls))
          else (prompt (); aux (line::ls))
      | None -> prompt(); aux(line::ls)
    in aux []
  ;;        

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    try
      let tm = s token (from_string (read_input ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

