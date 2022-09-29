open Lang

let print_fns =
  let go (f, xs, e) =
    Printf.printf "\tlet %s(%s) =\n" f Util.(join_mapped id xs);
    Anf.print 2 e

  in function
    | first :: rest ->
      go first;
      List.iter (fun x -> print_newline (); go x) rest
    | [] -> ()

let print_module mdl =
  let str = Llvm.string_of_llmodule mdl in
  let lines = String.split_on_char '\n' str in
  List.iter (fun s -> print_endline ("\t" ^ s)) lines

let () =
  (* let e = Lam ("x", Lam ("y", Bop (Add, Var "x", Var "y"))) in *)
  let e = App (App (Lam ("x", Lam ("y", If (Var "x", Var "y", Int 0))), Int 111), Int 222) in
  (* let e = If (Int 1, If (Int 2, Int 12, Int 10), Int 0) in *)
  (* let e = App (Lam ("x", Var "x"), Int 2) in *)
  
  print_endline "expression:";
  print e;
  
  print_endline "\nalpha-conversion:";
  let e = Alpha.convert e in
  print e;
  
  print_endline "\na-normalization:";
  let e = Norm.normalize e in
  Anf.print 1 e;
  

  print_endline "\nclosure-conversion:";
  let e = Close.convert e in
  Anf.print 1 e;

  print_endline "\nhoisting:";
  let fs = Hoist.hoist e in
  print_fns fs;

  print_endline "\nlowering:";
  let ir = Lower.lower fs in
  print_module ir;

  print_endline "executing:";
  let result = Jit.exec ir in
  Printf.printf "\tresult: %s\n" (Int64.to_string result)
