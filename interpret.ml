open Ast
open Printf

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)


type note = {
    mutable pitch : int;
    mutable octave : int;
    mutable duration : int;
}

type chord = {
    mutable notelist : note list;
    mutable duration : int;
}

type scale = {
    mutable notelist : note list;
}

(*Assumes notes in a stanza are auto-converted to chords*)
type stanza = {
    mutable chordlist : chord list;
}

type score = {
    mutable stanzalist : stanza list;
}

type cbtype =   Int of int

let getType v =
    match v with
        Int(v) -> "int"

let getInt v =
    match v with
        Int(v) -> v
        | _ -> 0

let initIdentifier t =
  match t with
    "int" -> Int(0)

exception ReturnException of cbtype * cbtype NameMap.t

let func_decls = NameMap.empty
let globals = NameMap.empty
let csv = ""

let rec eval env = function
    Id(name) -> print_string ("I am an id with name: " ^ name ^ "\n")
    | MemberAccess(vname, memname) -> print_string ("I am a member access on var: " ^ vname ^ " member: " ^ memname ^ "\n")
    | IntLiteral(i) -> print_string ("I am an intliteral: " ^ (string_of_int i) ^ "\n")
    | NoteConst(s) -> print_string ("I am a note constant: " ^ s ^ "\n")
    | BoolLiteral(b) -> print_string ("I am a bool literal: " ^ (string_of_bool b) ^ "\n")
    | Assign(toE, fromE) -> 
      let e1_data_type = get_data_type locals globals func_decls varName in
      let e2_data_type = get_data_type locals globals func_decls e in
      let v1 = string_of_expr locals globals func_decls varName in
      let v2 = string_of_expr locals globals func_decls e
      in
        (* Check to make sure what is being assigned is the correct datatype *)
        if (e2_data_type = "element") or (e2_data_type = "scan")
        then
          if (NameMap.mem v1 locals) or (NameMap.mem v1 globals)
          then v1 ^ (" = " ^ v2)
          else raise (Failure ("undeclared identifier " ^ v1))
        else
          if (e1_data_type = "bool") && (e2_data_type = "int")
          then
            if (NameMap.mem v1 locals) or (NameMap.mem v1 globals)
            then v1 ^ (" = " ^ v2)
            else raise (Failure ("undeclared identifier " ^ v1))
          else
            if e1_data_type <> e2_data_type
            then
              raise
                (Failure
                   ("incompatible datatype during assignment. Expecting " ^
                      (e1_data_type ^
                         (" but " ^ (e2_data_type ^ " is found")))))
            else
              if (NameMap.mem v1 locals) or (NameMap.mem v1 globals)
              then
                if e1_data_type = "card"
                then v1 ^ ("->setname(\"" ^ (v2 ^ "\")"))
                else
                  if e1_data_type = "list"
                  then "@" ^ ((get_name v1) ^ (" = " ^ v2))
                  else v1 ^ (" = " ^ v2)
              else raise (Failure ("undeclared identifier " ^ v1))
    | NoteExpr(s,e,e1) -> print_string ("I am a note expression: " ^ s ^ "," ^ "\n")
    | ChordExpr(el, e) -> print_string ("I am a chord expression: \n")
    | ListExpr(el) -> print_string ("I am a list epxression\n"); 
    | BinOp(e1,o,e2) -> print_string ("I am a binary operator\n")
    | UnaryOp(uo,e) -> print_string ("I am a unary operation\n")
    | MethodCall(s,el) -> print_string ("I am a method call on: " ^ s ^ "\n")
    | NoExpr -> print_string ("I am nothingness\n"); 

(* Main entry point: run a program *)
let rec run prog = match prog with
    [] -> print_string "Fuck it I'm done\n"
    | head::tail ->
        match head with
        VDecl(head) -> print_string ("Variable Declaration: " ^ head.varname ^ "\n"); (NameMap.add head.varname (initIdentifier "int") globals); run tail
        | FullDecl(head) -> print_string ("Full Declaration: " ^ head.fvname ^ "\n"); run tail
        | MDecl(head) -> print_string ("Method Declaration: " ^ head.fname ^ "\n"); (NameMap.add head.fname head func_decls); run tail
        | Stmt(head) -> match head with
                        Expr(e) -> (eval (NameMap.empty, globals) e);
                                    run tail
                        | Return(e) -> print_string ("I am an a return statement" ^ "\n"); run tail
                        | Block(sl) -> print_string ("I am a block statement" ^ "\n"); run tail
                        | If(e, sl, s1, s2) -> print_string ("I am a if statement" ^ "\n"); run tail
                        | ElseIf(e, sl) -> print_string ("I am a elseif statement" ^ "\n"); run tail
                        | Foreach(p, a, sl) -> print_string ("I am a foreach statement" ^ "\n"); run tail
                        | While(e, sl) -> print_string ("I am a while statement" ^ "\n"); run tail
                        | _ -> raise (Failure ("Unable to match the statment "))


