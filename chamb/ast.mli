type op = Add | Sub | Times | Div
type expr =
	Binop of expr * op * expr
	| Lit of int
	| Inv of expr
	| Note of char * int * int 