let _ =
  let lexbuf = Lexing.from_channel stdin in
  let methodized = "meth void program()\n" ^ lexbuf ^ "\nend\n"
  let program = Parser.program Scanner.token methdized in
  ignore (Interpret.run program)
