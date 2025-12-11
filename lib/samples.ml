open! Import

(* FIXME: make a type out of this tuple. for that, refactor the samples-reservoir interaction. *)
type sample = { id : int; sample : Location.t * Ppxlib.Longident.t option }

type t = {
  samples : sample list;
  file : File.t;
  query_type : Merlin.Query_type.t;
}

let file_traversal ~update_reservoir query_type =
  object
    inherit [bool] Ast_traverse.fold as super

    (* Possible FIXME: a longident such as [M.f] is only taken into account once all together as opposed to splitting it into [M] and [f]. to fix that, the parsing of the longident would have to be done manually (as opposed to further recursing it) in order to remember their individual location, which isn't reflected in the AST node*)
    method! longident_loc { loc = new_loc; txt = new_longident } in_vb =
      if Merlin.Query_type.(has_target query_type Longident) then
        update_reservoir (new_loc, Some new_longident);
      in_vb

    method! expression e in_vb =
      if Merlin.Query_type.(has_target query_type Expression) then
        update_reservoir (e.pexp_loc, None);
      super#expression e in_vb

    method! module_expr e in_vb =
      if Merlin.Query_type.(has_target query_type Module_expr) then
        update_reservoir (e.pmod_loc, None);
      super#module_expr e in_vb

    method! module_type_declaration d in_vb =
      if Merlin.Query_type.(has_target query_type Module_type_decl) then
        update_reservoir (d.pmtd_loc, None);
      super#module_type_declaration d in_vb

    method! module_declaration d in_vb =
      if Merlin.Query_type.(has_target query_type Module_decl) then
        update_reservoir (d.pmd_loc, None);
      super#module_declaration d in_vb

    method! value_binding vb _ = super#value_binding vb true

    method! pattern p in_vb =
      match
        ( Merlin.Query_type.(has_target query_type Var_pattern),
          in_vb,
          p.ppat_desc )
      with
      | true, false, Ppat_var { txt = _; loc } ->
          update_reservoir (loc, None);
          super#pattern p false
      | _ -> super#pattern p false
  end

let generate ~sample_size ~id_counter file query_type =
  match File.parse_impl file with
  | exception _ -> None
  | ast ->
      (* FIXME: filter out locations that would return an error anyways (possibly similar to how patterns are filtered out for [case-analysis] if they belong to a value binding) *)
      (* TODO: add info to each sample about what kind of node it corresponds to (interesting for queries with more than one possible type of node) *)
      let random_state = Reservoir.Random_state.make file in
      let reservoir =
        Reservoir.init ~placeholder:(Location.none, None) ~random_state
          sample_size
      in
      let update_reservoir = Reservoir.update ~random_state reservoir in
      let traverser = file_traversal ~update_reservoir query_type in
      let _ =
        let is_in_value_binding = false in
        traverser#structure ast is_in_value_binding
      in
      let samples =
        let make_sample ~id sample = { id; sample } in
        Reservoir.get_samples ~make_sample ~id_counter reservoir
      in
      Some ({ samples; file; query_type }, id_counter + sample_size)

let analyze ~init_cache ~merlin ~repeats ~update ~filter_outliers
    { samples; file; query_type } =
  let open Result.Syntax in
  if List.is_empty samples then
    Error
      (Logs.Log
         (Format.sprintf "File %s: there are no samples for query [%s]."
            (Yojson.Safe.to_string @@ File.yojson_of_t file)
            (Merlin.Query_type.to_string query_type)))
  else
    let* () = if init_cache then Merlin.init_cache file merlin else Ok () in
    let rec loop samples =
      match samples with
      | [] -> Ok ()
      | { id; sample = loc, li } :: rest ->
          let* cmd = Merlin.Cmd.make ~query_type ~file ?li ~loc merlin in
          let* responses = Merlin.Cmd.run ~repeats cmd in
          let fltr_outliers responses =
            let open Merlin.Response in
            (* If init_cache is false, then we don't want to filter the first response as it
               will be ran on cold cache. We don't count it as an outlier. *)
            let dropped_init =
              if init_cache then responses else List.tl responses
            in
            let sorted =
              List.sort (fun x y -> get_timing x - get_timing y) dropped_init
            in
            let filter responses =
              match responses with
              | [] -> []
              | x :: [] -> [ x ]
              | x :: y :: tl as lst ->
                  if get_timing x > 5 * get_timing y then y :: tl else lst
            in

            if init_cache then filter sorted
              (* We bring back dropped init response *)
            else List.hd responses :: filter sorted
          in
          let responses =
            if filter_outliers then fltr_outliers responses else responses
          in
          update { Data.id; responses; cmd; file; loc; query_type };
          loop rest
    in
    loop samples
