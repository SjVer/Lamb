open Lang

let () =
  (* \x . \x . x + x *)
  let e = Lam ("x", Lam ("x", Bop (Add, Var "x", Var "x"))) in

  print_endline "expression:";
  print e;
  
  let e' = Alpha.convert e in
  print_endline "\nalpha-conversion:";
  print e';

