(* Semantically-checked Abstract Syntax Tree and functions for printing it *)
(*   Authors: Jeevan Farias, Madeleine Tipp, Daniel Mesko *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SArrayLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SField of string * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SAccess of string * sexpr
  | SArrayAssign of string * sexpr * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SVDecl of typ * string
  | SVDeclAssign of typ * string * sexpr
  | SADecl of typ * string * int 
  | SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SCharLit(l) -> String.make 1 l
  | SStringLit(l) -> l
  | SArrayLit(arr) -> "[" ^ (List.fold_left (fun lst elem -> lst ^ " " ^ string_of_sexpr elem ^ ",") "" arr) ^ "]"
  | SField(e,f) -> e ^ "." ^ string_of_sexpr f
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SAccess(a, i) -> a ^ "[" ^ string_of_sexpr i ^ "]"
  | SArrayAssign(arr, index, rval) -> arr ^ "[" ^ string_of_sexpr index ^ "]" ^ string_of_sexpr rval
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""  ) ^ ")"     

let rec string_of_sstmt = function
    SVDecl(t, i) -> string_of_typ t ^ " " ^ i ^ "\n"
  | SVDeclAssign(t, i, e) -> string_of_typ t ^ " " ^ i ^ " = " ^ string_of_sexpr e ^ "\n"
  | SADecl(t, i, s) -> string_of_typ t ^ " " ^ i ^ "[" ^ string_of_int s ^ "]\n"
  | SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.slocals) ^ *)
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
