open Anf
open Fresh

module SMap = Map.Make(String)

let ctx = Llvm.create_context ()
let bld = Llvm.builder ctx
let mdl = Llvm.create_module ctx "module"
let i64 = Llvm.i64_type ctx
let cls = Llvm.type_by_name

let get_var env v = Printf.eprintf "get_var %s\n" v; SMap.find v env
let get_glob g = Option.get (Llvm.lookup_function g mdl)

let int_to_ptr value name =
  Llvm.build_inttoptr value (Llvm.pointer_type i64) name bld

let lower_atom env = function
  | Int i -> Llvm.const_int i64 i
  | Var v -> Llvm.build_load (get_var env v) v bld
  | Glob g ->
    print_endline ("lowering glob " ^ g);
    let g' = Llvm.build_load (get_glob g) g bld in
    Llvm.build_ptrtoint g' i64 g bld

let rec lower_expr env = function
  | Return a -> Llvm.build_ret (lower_atom env a) bld

  | Tuple (d, es, _b) ->
    let _ptr = fresh "tup" in
    let ty = Llvm.array_type i64 (List.length es) in
    let tup = Llvm.build_malloc ty d bld in
    
    let set i v =
      let index = Array.make 1 (Llvm.const_int i64 i) in
      let ptr = fresh "ptr" in
      let gep = Llvm.build_in_bounds_gep tup index ptr bld in
      ignore (Llvm.build_store v gep bld)
    in
    List.iteri (fun i e -> set (i) (lower_atom env e)) es;
    tup
    
  | Get (d, t, i, b) ->
    Llvm.build_unreachable bld
    (* let index = Array.make 1 (Llvm.const_int i64 i) in
    let tup = int_to_ptr (get_var env t) (fresh "tup") in
    let ptr = fresh "ptr" in
    print_endline (Llvm.string_of_llvalue tup);

    let gep = Llvm.build_in_bounds_gep tup index ptr bld in
    print_endline (Llvm.string_of_llvalue gep); *)

    (* let v = Llvm.build_load gep d bld in
    print_string (Llvm.string_of_llvalue v); v *)

  | _ -> Llvm.build_unreachable bld
    (* failwith "Invalid expression survived hoisting phase!" *)

let lower_fn (f, xs, (_, _, e), _js) =
  (* create function *)
  let ty = Llvm.function_type i64 (Array.make (List.length xs) i64) in
  let fn = Llvm.define_function f ty mdl in

  (* gather args *)
  let params = Array.to_list (Llvm.params fn) in
  let rec bind env = function
    | x :: xs, v :: vs -> bind (SMap.add x v env) (xs, vs)
    | _ -> env
  in let env = bind SMap.empty (xs, params) in

  (* build blocks *)
  let entry = Llvm.entry_block fn in
  Llvm.position_at_end entry bld;
  ignore (lower_expr env e)

let lower fns =
  List.iter lower_fn fns;
  Llvm.print_module "/dev/stdout" mdl