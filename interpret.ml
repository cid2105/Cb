open Ast
open Printf

module NameMap = Map.Make(struct
    type t = string
    let compare x y = Pervasives.compare x y
end)

(*
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
*)
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

(* Main entry point: run a program *)
let rec run prog = match prog with
    [] -> print_string "Fuck it I'm done\n"
    | head::tail ->
        match head with
        VDecl(head) -> print_string ("Variable Declaration: " ^ head.varname ^ "\n"); (NameMap.add head.varname (initIdentifier "int") globals); run tail
        | FullDecl(head) -> print_string ("Full Declaration: " ^ head.fvname ^ "\n"); run tail
        | MDecl(head) -> print_string ("Method Declaration: " ^ head.fname ^ "\n"); (NameMap.add head.fname head func_decls); run tail
        | Stmt(head) -> match head with
                        Expr(e) -> print_string ("I am an expression statement" ^ "\n");
                                    match e with
                                        Id(name) -> print_string ("I am an id with name: " ^ name ^ "\n");
                                        | MemberAccess(vname, memname) -> print_string ("I am a member access on var: " ^ vname ^ " member: " ^ memname ^ "\n");
                                        | IntLiteral(i) -> print_string ("I am an intliteral: " ^ i) ^ "\n";
                                        | Noteconst(s) -> print_string ("I am a note constant: " ^ s ^ "\n");
                                        | BoolLiteral(b) -> print_string ("I am a bool literal: " ^ b ^ "\n");
                                        | DurConst(s) -> print_string ("I am a duration constant: " ^ s ^ "\n");
                                        (*TODO*)
                                run tail
                        | Return(e) -> print_string ("I am an a return statement" ^ "\n"); run tail
                        | Block(sl) -> print_string ("I am a block statement" ^ "\n"); run tail
                        | If(e, sl, s1, s2) -> print_string ("I am a if statement" ^ "\n"); run tail
                        | ElseIf(e, sl) -> print_string ("I am a elseif statement" ^ "\n"); run tail
                        | Foreach(p, a, sl) -> print_string ("I am a foreach statement" ^ "\n"); run tail
                        | While(e, sl) -> print_string ("I am a while statement" ^ "\n"); run tail
                        | _ -> raise (Failure ("Unable to match the statment "))