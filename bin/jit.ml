module LlEE = Llvm_executionengine

let lines str = List.length (String.split_on_char '\n' str)

let exec mdl =
  let lines_before = lines (Llvm.string_of_llmodule mdl) in
  
  let pm = Llvm.PassManager.create () in
  Llvm_scalar_opts.(
    add_instruction_combination pm;
    add_reassociation pm;
    add_basic_alias_analysis pm;
    add_dead_store_elimination pm;
    add_cfg_simplification pm;
    add_gvn pm;
  );

  let pmbld = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level 3 pmbld;
  Llvm_passmgr_builder.set_size_level 3 pmbld;
  Llvm_passmgr_builder.populate_module_pass_manager pm pmbld;
  
  ignore (Llvm.PassManager.run_module mdl pm);
  Llvm_analysis.assert_valid_module mdl;

  let lines_after = lines (Llvm.string_of_llmodule mdl) in
  Printf.printf "\toptimized from %d to %d lines (removing %d)\n"
    lines_before lines_after (lines_before - lines_after);
  
  assert (LlEE.initialize ());
  let ee = LlEE.create mdl in
  let ty = Foreign.funptr Ctypes.(void @-> returning int64_t) in
  let fptr = LlEE.get_function_address "main" ty ee in
  fptr ()
