(* File calc.ml *)
open Ast

(* list of Ocaml notes *)
(* http://www.siteduzero.com/tutoriel-3-243632-de-la-musique-avec-caml.html#ss_part_2 *)
(*to run it, ex: Note(A, 500) *)
let do1 = 65 and do2=131 and do3 = 262 and do4=523 and  dod1 = 69 and dod2 = 139 and dod3 = 277 and dod4 = 554 
and  re1 = 73 and re2 = 147 and re3 = 294 and re4 = 587 
and  mib1 = 78 and mib2 = 156 and mib3 = 311 and mib4 = 622 
and  mi1 = 82 and mi2 = 165 and mi3 = 330 and mi4 = 659 
and  fa1 = 87 and fa2 = 175 and fa3 = 349 and fa4 = 698 
and  fad1 = 92 and fad2 = 185 and fad3 = 370 and fad4 = 740 
and  sol1 = 98 and sol2 = 196 and sol3 = 392 and sol4 = 784 
and  sold1 = 104 and sold2 = 208 and sold3 = 415 and sold4 = 830 
and  la1=110 and la2 = 220 and la3=440 and la4 = 880 
and  sib1 = 117 and sib2 = 233 and sib3 = 466 and sib4 = 932 
and  si1 = 123 and si2 = 247 and si3 = 494 and si4 = 988 
and  do5 = 1046

let rec eval = function
	Lit(x) -> x
	| Inv(e1) -> - (eval e1)
	| Note(c, x, f) ->
	    begin
	    match c with
	        'A' -> Graphics.sound la3 x; x;
	        |'B' -> Graphics.sound si3 x; x;
	        |'C' -> Graphics.sound do3 x; x;
            |'D' -> Graphics.sound re3 x; x;
            |'E' -> Graphics.sound mi3 x; x;
            |'F' -> Graphics.sound fa3 x; x;
            |'G' -> Graphics.sound sol3 x; x
            | _ -> 0
        end
	| Binop(e1, op, e2) ->
	let v1 = eval e1 and v2 = eval e2 in
	match op with
	Add -> v1 + v2
	| Sub -> v1 - v2
	| Times -> v1 * v2
	| Div -> v1 / v2
let _=
  	try
   	    (*	List.map (fun x -> eval (Parser.program Scanner.token 
    							(Lexing.from_string String.make(x))) ["Note(A, 500, 0)",  "Note(B, 500, 0)", "Note(C, 500, 0)"];
   	    	0;*)
       	let lexbuf = Lexing.from_channel stdin in
    	while true do
      		let expr = Parser.program Scanner.token lexbuf in
      		let result = eval expr in
      		print_int result; print_newline(); flush stdout
    	done
    with Scanner.Eof ->
        exit 0