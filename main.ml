open Printf;;


open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let rec updateTheContext ctx context = match context with
  (x,t,ty)::tail -> updateTheContext([(x,ty)]@ctx)(tail)
  | [] -> ctx;;

let rec removeFromContext (x',ty') context context2 = match context with
   (x,ty)::tail -> if ((compare x x') == 0) then removeFromContext(x',ty')(tail)(context2) else removeFromContext(x',ty')(tail)([(x,ty)]@context2)
   | [] -> context2;;

let rec updateContext ctx context = match context with
  (x,t,ty)::tail -> updateContext([(x,ty)]@( removeFromContext(x,ty)(ctx)([]) ))(tail)
  | [] -> ctx;;




let top_level_loop () =
  let l = Array.length Sys.argv in
  let ic = if l = 2 && Sys.argv.(1) <> "--debug" || l = 3 && Sys.argv.(1) <> "--debug" then
          open_in Sys.argv.(1) 
           else stdin in
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx current_context =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (input_line ic)) in
      let tyTm = typeof(ctx)(tm) in
          if l = 2 && Sys.argv.(1) = "--debug" || l = 3 && Sys.argv.(2) = "--debug"
            then match eval tm current_context true with
		(tm',context) -> print_endline (string_of_term (tm') ^ " : " ^ string_of_ty tyTm);loop (updateContext [] context) (context);
            else match eval tm current_context false with
		(tm',context) -> print_endline (string_of_term (tm') ^ " : " ^ string_of_ty tyTm);loop (updateContext [] context) (context);



    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx(current_context)
     | Parse_error ->
         print_endline "syntax error";
         loop ctx(current_context)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx(current_context)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx []
  ;;

top_level_loop ()
;;

