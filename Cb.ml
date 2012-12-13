open Printf

let _ =
    let lexbuf = Lexing.from_string "test1 = 5;" in
    let program = Parser.program Scanner.token lexbuf in
    ignore (Interpret.run (List.rev program))
