open! Import

let analyze ~backend:(module Backend : Backend.Data_tables) ~repeats
    ~cache_workflow ~merlin_path ~proj_dirs ~data_dir ~sample_size ~query_types
    ~filter_outliers ~extensions =
  let merlin_path = Fpath.v merlin_path in
  let merlin = Merlin.make merlin_path cache_workflow in
  let proj_path dir = Fpath.v @@ dir in
  let open Result.Syntax in
  let* data_dir =
    match data_dir with
    | Some dir -> Ok (Fpath.v dir)
    | None ->
        let+ proj_name =
          match proj_dirs with
          | [] ->
              Error (`Msg "Expected at least one file or project to analyze")
          | [ proj_dir ] -> Ok (Fpath.basename @@ proj_path proj_dir)
          | proj_dir :: l ->
              let num = List.length l in
              let base = Fpath.basename @@ proj_path proj_dir in
              Ok (Format.sprintf "%s+%iothers" base num)
        in
        let ts = Int.to_string @@ Int.of_float @@ Unix.time () in
        Fpath.v ("data/" ^ proj_name ^ "+" ^ ts)
  in
  let module D = Data.Make (Backend) in
  let data = D.init merlin data_dir in
  let init_cache = D.init_cache data in
  let proj_paths = List.map proj_path proj_dirs in
  let* files = File.get_files ~extensions proj_paths in
  (*TODO: add terminal logging when getting the files: log number of files that are going to be benchmarked and, at the end, log how many that are.*)
  let side_effectively_add_data id_counter (file, query_type) =
    let update = D.update data in
    if Merlin.Query_type.is_global query_type then
      let () =
        let d =
          let* cmd = Merlin.Cmd.make ~query_type ~file merlin in
          let* responses = Merlin.Cmd.run ~repeats cmd in
          Ok (cmd, responses)
        in
        match d with
        | Error log -> D.persist_logs ~log data
        | Ok (cmd, responses) ->
            update
              {
                Data.id = id_counter;
                responses;
                cmd;
                file;
                loc = Location.none;
                query_type;
              }
      in
      id_counter + 1
    else
      match Samples.generate ~sample_size ~id_counter file query_type with
      | None ->
          let log =
            Logs.Warning
              (Format.sprintf "File %s couldn't be parsed and was ignored.\n"
                 (Yojson.Safe.to_string @@ File.yojson_of_t file))
          in
          D.persist_logs ~log data;
          id_counter
      | Some (samples, new_id_counter) -> (
          match
            Samples.analyze ~init_cache ~merlin ~repeats ~update
              ~filter_outliers samples
          with
          | Ok () -> new_id_counter
          | Error log ->
              D.persist_logs ~log data;
              new_id_counter)
  in
  let _last_sample_id =
    (* The traversal is done in files -> query_types order. *)
    List.fold_over_product ~l1:files ~l2:query_types ~init:0
      side_effectively_add_data
  in
  D.dump data;
  D.wrap_up data ~proj_paths;
  if Merlin.is_server merlin then Merlin.stop_server merlin else ();
  Ok ()
