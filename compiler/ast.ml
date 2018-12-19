(* SSOL Abstract Syntax Tree and functions for printing it 
    Authors: Jeevan Farias, Madeleine Tipp, Daniel Mesko *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Pipend

type uop = Neg | Not

type typ = Int | Bool | Float | Void | Char | String | Point | Curve | Canvas 
               | Array of typ * int

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | ArrayLit of expr list 
  | Id of string
  | Binop of expr * op * expr
  | Field of string * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Access of string * expr
  | ArrayAssign of string * expr * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    VDecl of typ * string
  | VDeclAssign of typ * string * expr
  | ADecl of typ * string * int
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Pipend -> "|="

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "String"
  | Point -> "Point"
  | Curve -> "Curve"
  | Canvas -> "Canvas"
  | Array(t, n) -> (string_of_typ t) ^ "[" ^ (string_of_int n) ^ "]"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(l) -> String.make 1 l
  | StringLit(l) -> l
  | ArrayLit(arr) -> "[" ^ (List.fold_left (fun lst elem -> lst ^ " " ^ string_of_expr elem ^ ",") "" arr) ^ "]"
  | Field(s,f) -> s ^ "." ^ string_of_expr f
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Access(a, i) -> a ^ "[" ^ string_of_expr i ^ "]"
  | ArrayAssign(arr, index, rval) -> arr ^ "[" ^ string_of_expr index ^ "]" ^ string_of_expr rval
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

  let rec string_of_stmt = function
    VDecl(t, i) -> string_of_typ t ^ " " ^ i ^ "\n"
  | VDeclAssign(t, i, e) -> string_of_typ t ^ " " ^ i ^ " = " ^ string_of_expr e ^ "\n"
  | ADecl(t, i, s) -> string_of_typ t ^ " " ^ i ^ "[" ^ string_of_int s ^ "]\n"
  | Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
