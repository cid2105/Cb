type action = Ast | Interpret

(** find out how to call java from inside ml? *)

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)

let _ =
  let action =
  if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
                    ("-i", Interpret) ]
  else Interpret in
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
      match action with
        Ast -> let listing = Ast.string_of_program program
              in print_string listing
  (*| Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing*)
        | Interpret -> ignore (Interpret.helper (List.rev program))
