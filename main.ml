(*
  Authors: Lucas Grandal Lama, Sergio Puertas Pérez.
*)

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
      let c = s token (from_string(read_input()) ) in
      loop (execute ctx c)
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
         print_endline "...pescao!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

