open Printf

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
    ignore (Interpret.helper (List.rev program))
