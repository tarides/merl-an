open! Import

let analyze ~backend ~repeats ~cache_workflow ~merlin_path ~proj_dirs ~data_dir
    ~per_file_samples ~total_samples ~query_types ~filter_outliers ~extensions
    ~force_yes ~verbose =
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
  let proj_paths = List.map proj_path proj_dirs in
  let* all_files = File.get_files ~extensions proj_paths in
  let files =
    match total_samples with
    | None -> all_files
    | Some total ->
        let num_files = total / per_file_samples in
        let random_state = Reservoir.Random_state.make "chocolate" in
        let reservoir =
          Reservoir.init ~placeholder:(List.hd all_files) ~random_state
            num_files
        in
        let () =
          List.iter (Reservoir.update ~random_state reservoir) all_files
        in
        let make_sample ~id:_ file = file in
        Reservoir.get_samples ~make_sample ~id_counter:1 reservoir
  in
  let* channels = Data.init ~backend ~force_yes merlin data_dir in
  Fun.protect
    ~finally:(fun () ->
      Data.wrap_up channels ~proj_paths;
      if Merlin.is_server merlin then Merlin.stop_server merlin else ())
    (fun () ->
      let init_cache = Data.init_cache channels in
      let stream_query_data id_counter (file, query_type) =
        let write = Data.write channels in
        if Merlin.Query_type.is_global query_type then
          let () =
            let d =
              let* cmd = Merlin.Cmd.make ~query_type ~file merlin in
              let* responses = Merlin.Cmd.run ~verbose ~repeats cmd in
              Ok (cmd, responses)
            in
            match d with
            | Error log -> Data.persist_logs ~log channels
            | Ok (cmd, responses) ->
                write
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
          match
            Samples.generate ~per_file_samples ~id_counter file query_type
          with
          | None ->
              let log =
                Logs.Warning
                  (Format.sprintf
                     "File %s couldn't be parsed and was ignored.\n"
                     (Yojson.Safe.to_string @@ File.yojson_of_t file))
              in
              Data.persist_logs ~log channels;
              id_counter
          | Some (samples, new_id_counter) -> (
              match
                Samples.analyze ~init_cache ~merlin ~repeats ~stream:write
                  ~filter_outliers ~verbose samples
              with
              | Ok () -> new_id_counter
              | Error log ->
                  Data.persist_logs ~log channels;
                  new_id_counter)
      in
      let _last_sample_id =
        (* The traversal is done in files -> query_types order. *)
        List.fold_over_product ~l1:files ~l2:query_types ~init:0
          stream_query_data
      in
      Ok ())
