open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
    let sentences = Parser.sentences Lexer.main (Lexing.from_channel stdin) in
    let newenv = List.fold_left (
      fun env sentence ->
        let (id, newenv, v) = eval_sentence env sentence in
          Printf.printf "val %s = " id;
          pp_val v;
          print_newline();
          newenv
    ) env sentences in

    read_eval_print newenv
  with
    e -> print_string @@ Printexc.to_string e;
    print_newline();
    read_eval_print env
;;

let initial_env = 
  Environment.extend "iv" (IntV 4) @@
  Environment.extend "iii" (IntV 3) @@
  Environment.extend "ii" (IntV 2) @@
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10) Environment.empty))

let _ = read_eval_print initial_env
