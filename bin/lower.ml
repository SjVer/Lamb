open Anf

module SMap = Map.Make(String)

let ctx = Llvm.create_context ()
let bld = Llvm.builder ctx
let mdl = Llvm.create_module ctx "module"
let i64 = Llvm.i64_type ctx
let malloc =
  let fty = Llvm.function_type (Llvm.pointer_type i64) [|i64|] in
  Llvm.declare_function "malloc" fty mdl
let func_ty arity =
  let args = Array.make arity i64 in
  Llvm.function_type i64 args
let tuple_ty len =
  let els = Array.make len i64 in
  Llvm.struct_type ctx els

let get_const i = Llvm.const_int i64 i
let get_var env v = SMap.find v env
let get_func_ptr g = Option.get (Llvm.lookup_function g mdl)


let lower_atom env = function
  | Int i ->get_const i
  | Var v -> get_var env v
  | Glob g -> Llvm.build_ptrtoint (get_func_ptr g) i64 "casted" bld

let lower_bop =
  let open Lang in function
  | Add -> Llvm.build_add
  | Sub -> Llvm.build_sub
  | Mul -> Llvm.build_mul
  | Div -> Llvm.build_sdiv

let rec lower_expr env = function
  | Return a -> Llvm.build_ret (lower_atom env a) bld

  | Break a ->
    let bb = SMap.find "\b" env in
    let bb' = Llvm.block_of_value bb in
    let _ = Llvm.build_br bb' bld in
    lower_atom env a

  | Tuple (d, es, b) ->
    (* malloc the tuple *)
    let size = get_const (List.length es * 8) in
    let tup = Llvm.build_call malloc [|size|] "tup" bld in
    (* let tup = Llvm.build_malloc (tuple_ty (List.length es)) "tup" bld in *)
    
    (* set each of the tuple's items *)
    let set i v =
      let gep = Llvm.build_gep tup [|get_const i|] "gep" bld in
      ignore (Llvm.build_store v gep bld);
    in
    List.iteri (fun i e -> set i (lower_atom env e)) es;

    (* convert the tuple to an i64 so it can be used *)
    let tup' = Llvm.build_ptrtoint tup i64 d bld in
    lower_expr (SMap.add d tup' env) b
    
  | App (d, f, es, b) ->
    let es' = List.map (lower_atom env) es in
    let ptr = get_var env f in
    let fty = Llvm.pointer_type (func_ty (List.length es)) in
    let func = Llvm.build_inttoptr ptr fty "func" bld in

    let res = Llvm.build_call func (Array.of_list es') "res" bld in
    lower_expr (SMap.add d res env) b
  
  | Bop (d, op, x, y, b) ->
    let build_op = lower_bop op in
    let x' = lower_atom env x in
    let y' = lower_atom env y in
    let r = build_op x' y' "bopr" bld in
    lower_expr (SMap.add d r env) b

  | If (d, c, t, e, b) ->
    let currf = Llvm.block_parent (Llvm.insertion_block bld) in
    let tbb = Llvm.append_block ctx "then" currf in
    let ebb = Llvm.append_block ctx "else" currf in
    let pbb = Llvm.append_block ctx "phi" currf in

    let c' = Llvm.build_is_not_null (lower_atom env c) "cond" bld in
    let _ = Llvm.build_cond_br c' tbb ebb bld in
    let env' = SMap.add "\b" (Llvm.value_of_block pbb) env in

    Llvm.position_at_end tbb bld;
    let t' = lower_expr env' t in
    let tbb' = Llvm.insertion_block bld in
    
    Llvm.position_at_end ebb bld;
    let e' = lower_expr env' e in
    let ebb' = Llvm.insertion_block bld in
    
    Llvm.move_block_after ebb' pbb;
    Llvm.position_at_end pbb bld;
    let p = Llvm.build_phi [t', tbb'; e', ebb'] "phi" bld in
    lower_expr (SMap.add d p env) b

  | Get (d, t, i, b) ->
    let int_to_ptr v n = Llvm.build_inttoptr v (Llvm.pointer_type i64) n bld in
    let tup = int_to_ptr (get_var env t) "tup" in
    
    let gep = Llvm.build_gep tup [|get_const i|] "ptr" bld in

    let v = Llvm.build_load gep d bld in
    lower_expr (SMap.add d v env) b

  | _ -> Llvm.build_unreachable bld
    (* failwith "Invalid expression survived hoisting phase!" *)

let lower_fn (f, xs, e) =
  (* create function *)
  let ty = func_ty (List.length xs) in
  let fn = Llvm.define_function f ty mdl in

  (* gather args *)
  let params = Array.to_list (Llvm.params fn) in
  let rec bind env = function
    | x :: xs, v :: vs ->
      Llvm.set_value_name x v;
      bind (SMap.add x v env) (xs, vs)
    | _ -> env
  in let env = bind SMap.empty (xs, params) in

  (* build blocks *)
  let entry = Llvm.entry_block fn in
  Llvm.position_at_end entry bld;
  ignore (lower_expr env e)

let lower fns =
  List.iter lower_fn fns;
  mdl