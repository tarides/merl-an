let default_case () = print_endline "Unhandled command"

let exception_case () =
  let output =
    {| {
     "class": "exception",
     "value": "Not_found\nRaised at Ocaml_typing__Ident.find_same in file \"src/ocaml/typing/ident.ml\", line 250, characters 6-21\nCalled from Ocaml_typing__Env.IdTbl.find_same in file \"src/ocaml/typing/env.ml\", line 384, characters 10-40\nRe-raised at Ocaml_typing__Env.IdTbl.find_same in file \"src/ocaml/typing/env.ml\", line 389, characters 21-30\nCalled from Ocaml_typing__Env.find_type in file \"src/ocaml/typing/env.ml\" (inlined), line 1249, characters 2-24\nCalled from Merlin_analysis__Browse_tree.normalize_type_expr in file \"src/analysis/browse_tree.ml\", line 66, characters 28-52\nCalled from Merlin_analysis__Browse_tree.same_constructor.get_decls in file \"src/analysis/browse_tree.ml\", line 85, characters 17-57\nCalled from Merlin_analysis__Browse_tree.same_constructor in file \"src/analysis/browse_tree.ml\", line 99, characters 12-23\nCalled from Merlin_analysis__Browse_tree.all_constructor_occurrences.aux in file \"src/analysis/browse_tree.ml\", line 132, characters 28-70\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34\nCalled from Query_commands.dispatch.constructor_occurrence in file \"src/frontend/query_commands.ml\", line 838, characters 15-72\nCalled from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\", line 65, characters 15-53\nCalled from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 700, characters 8-12\nRe-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 702, characters 30-39\nCalled from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15\nRe-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24\nCalled from Stdlib__Fun.protect in file \"fun.ml\", line 33, characters 8-15\nRe-raised at Stdlib__Fun.protect in file \"fun.ml\", line 38, characters 6-52\nCalled from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38\nRe-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 20, characters 42-53\nCalled from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", line 106, characters 14-110\n"
     }|}
  in
  print_endline output

let () =
  let () =
    match Sys.argv.(1) with
    | "exception" -> exception_case ()
    | _ | (exception _) -> default_case ()
  in
  exit 0
