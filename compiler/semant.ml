(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)

  (* TODO: Passing "x" as element name - possible duplicate conflict?*)
	let built_in_decls = 
    let add_bind map (name, tylst) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = List.map (fun elem -> (elem, "x")) tylst;
      (* locals = []; *) body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", [Int]);
			                         ("printb", [Bool]);
			                         ("printf", [Float]);
			                         ("printbig", [Int]);
						 									 ("sprint", [String]);
	 														 ("draw", [String; String])]
  
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    (* check_binds "local" func.locals; *)

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  (*let check_dup_def name map =
    if StringMap.find name map then name else name
  in*)

	let check_assign lvaluet rvaluet err =
	   match lvaluet with
           Array(lt, _) ->
              (match rvaluet with
                  Array(rt, _) -> if lt == rt then lvaluet else raise (Failure err)
                | _ -> raise (Failure err))
         | _ -> if lvaluet == rvaluet then lvaluet else raise (Failure err)
    in   
  

	(* have recursive builder function for types *)
 (*	 	let build_memmap mems (ty, name) = match ty with
				Point -> StringMap.add name (ty, Some (build_memmap StringMap.empty (Float,"x"))) mems
			| Curve -> StringMap.add name (ty, Some (build_memmap StringMap.empty (Point, "x"))) mems (*[(Point, "ep1"); (Point, "ep2"); (Point, "cp1"); (Point, "cp2")])) *) 
			| Canvas -> StringMap.add name (ty, Some (build_memmap StringMap.empty (Point, "x"))) mems (*[(Float,"x"); (Float, "y")]))*) 
			|	_ -> StringMap.add name (ty, None) mems 
		in  *) 

	(* Create initial symbol map with globals and formals *)
		let globmap = 
			let rec build_memmap m (ty,name) = match ty with
					Point -> StringMap.add name (ty, Some StringMap.empty) m (*(List.fold_left build_memmap StringMap.empty [(Float, "x"); (Float, "y")])) m *)
				| Curve -> StringMap.add name (ty, Some (StringMap.empty)) m
				| Canvas -> StringMap.add name (ty, Some StringMap.empty) m (* (List.fold_left build_memmap StringMap.empty [(Float, "x"); (Float, "y")])) m *)
				| _ -> StringMap.add name (ty, None) m
			in 
			List.fold_left build_memmap StringMap.empty (globals @ func.formals (* @ func.locals *) )
    in

    (* Return a tuple of (typ, membermap) from supplied symbol table *)
    
		let type_of_identifier locals s =
      try fst (StringMap.find s locals)
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

		let member_map_of_identifier locals s =
			try (match (StringMap.find s locals) with
					(_, Some map) -> map
				| (ty, None) -> raise (Failure ("cannot access members of " ^ s ^ 
							": is of type " ^ string_of_typ ty)) )
			with Not_found -> raise (Failure ("undeclared identifier " ^ s))

		in
(* 
    let add_to_symbols name ty = 
      let res = List.find_opt name func.locals 
        in match res with 
          None -> raise(Failure "This variable is already defined")
          | _ -> func.locals :: [(name,ty)]
    in *)


    (* Return a semantically-checked expression, i.e., with a type *)

    let rec expr locals = function
        Literal  l  -> (Int, SLiteral l)
      | Fliteral l  -> (Float, SFliteral l)
      | BoolLit l   -> (Bool, SBoolLit l)
      | CharLit l   -> (Char, SCharLit l)
      | StringLit l -> (String, SStringLit l)
      | ArrayLit elist -> 
				  let rec typmatch t (ty, _) = 
						if t == ty then
		  				ty
						else
							raise (Failure ("array elements are not of same type")) 
   			  and slist = List.map (fun e -> expr locals e) elist in
					(match slist with
							[] -> raise (Failure "cannot have array literal with 0 elements")
						| _ -> 
							let ty = List.fold_left typmatch (fst (List.hd slist)) slist in 
							(Array(ty, List.length slist), SArrayLit(slist)) )
      | Noexpr      -> (Void, SNoexpr)
      | Id s        -> (type_of_identifier locals s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier locals var
          and (rt, e') = expr locals e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
	  | Access(arr, ind) ->
					let arrtyp = type_of_identifier locals arr 
					and (ityp, iex) as ind' = expr locals ind in
					(match arrtyp with
							Array(t, s) -> (match ityp with
									Int -> (t, SAccess(arr, ind'))
								|	_ -> raise (Failure ("expected Int for array index value, " ^ 
													"but was given " ^ string_of_sexpr ind')) )
				   	| _ -> raise (Failure ("cannot access index " ^ string_of_sexpr ind' ^ 
									" of " ^ arr ^ ": it has type " ^ string_of_typ arrtyp)) )
	  | ArrayAssign(arr, ind, ex) ->
		 			let arrtyp = type_of_identifier locals arr
				  and ind' = expr locals ind
					and ex'= expr locals ex in
					let err = "illegal assignment " ^ (string_of_typ arrtyp) ^ " = " ^ (string_of_typ (fst ex')) in
					(match arrtyp with
							Array(t, s) -> (check_assign t (fst ex') err, SArrayAssign(arr, ind', ex'))
						| _ -> raise (Failure (err)) )
	  | Field(obj, mem) -> 
					let smem = expr locals mem in
					(fst smem, SField(obj, smem))
      | Unop(op, e) as ex -> 
          let (t, e') = expr locals e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr locals e1 
          and (t2, e2') = expr locals e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
					| Mod when same && t1 = Int -> Int
					| Pipe when (t1 = Point || t1 = Curve) && 
							(t2 = Point || t2 = Curve) -> Canvas
					| Pipend when t1 = Canvas && (t2 = Point || t2 = Curve)	 -> Canvas
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr locals e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
			| Constructor(ty, exl) -> 
					let sxl = List.map (expr locals) exl in
					match ty with
							Point -> (ty, SConstructor(ty, sxl))
						| Curve -> (ty, SConstructor(ty, sxl))
						| Canvas -> (ty, SConstructor(ty, sxl))
						| x -> raise (Failure ("illegal type constructor call, " ^ string_of_typ x ^
									" is not a complex type"))
    in

    let check_bool_expr locals e = 
      let (t', e') = expr locals e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)

	(*TODO modify for change to symbol map structure of name -> (type,option Map) *)

		(* Statements within block not getting checked b/c they use the inner function?*)

    let rec check_stmt locals = function
      Block sl -> (* let ssl = List.map (fun s -> check_stmt locals s) sl in
          SBlock(ssl) *)
       let rec check_block block_locals ssl= function
         (*  s::ss -> ssl @ [(check_stmt block_locals s)] *)
        [Return _ as s] -> ssl @ [check_stmt block_locals s]
          | Return _::_ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> [check_stmt block_locals (Block sl)] @ (check_block block_locals ssl ss)
          | s :: ss -> 
            (match s with 
                VDecl(t,name) -> 
                (* TODO: CHECK FOR DUPLICATE *)
                  let block_locals = StringMap.add name (t, None) block_locals
                    in [check_stmt block_locals s] @ check_block block_locals ssl ss
              | VDeclAssign(t,name,e) ->
								let sx = expr block_locals e in
								let typ = (match fst(sx) with
												Array(tp,s) -> if tp == t 
													then Array(tp,s) 
													else raise(Failure("Array literal is of inconsistent type"))
											| _ -> if fst(sx) == t
													then fst(sx) 
													else raise(Failure("illegal assignment"))) in
								let block_locals = StringMap.add name (typ,None) block_locals in
                  [check_stmt block_locals s] @ check_block block_locals ssl ss
              | ADecl(t,name, n) -> 
                let block_locals = StringMap.add name (Array(t,n), None) block_locals
                  in [check_stmt block_locals s] @ check_block block_locals ssl ss
              | _ -> [check_stmt block_locals s] @ check_block block_locals ssl ss)
          | []  -> ssl 
        in SBlock(check_block locals [] sl)
		| VDecl(t,s) -> SVDecl(t,s)
	  | VDeclAssign(t,s,e) -> 
			let sx = expr locals e in
			let ty = type_of_identifier locals s in 
			SVDeclAssign(ty,s,sx)
	  | ADecl(t,s,n) -> SADecl(t,s,n) 
	  | Expr e -> SExpr (expr locals e)
      | If(p, b1, b2) -> SIf(check_bool_expr locals p, check_stmt locals b1, check_stmt locals b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr locals e1, check_bool_expr locals e2, expr locals e3, check_stmt locals st)
      | While(p, s) -> SWhile(check_bool_expr locals p, check_stmt locals s)
      | Return e -> let (t, e') = expr locals e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      (* slocals  = func.locals; *)
      sbody = match check_stmt globmap (Block func.body) with
	       SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
