open! Import

module Cache_workflow = struct
  type t =
    | Buffer_typed
    (* | File_hash_diff  *)
    (* | Cmis_cached  *)
    | No_cache
  [@@deriving yojson_of, enumerate]

  let to_string = function
    | Buffer_typed -> "buffer-typed"
    (* | File_hash_diff -> "file-hash-diff"  *)
    (* | Cmis_cached -> "cmis-cached" *)
    | No_cache -> "no-cache"

  let description = function
    | Buffer_typed -> "Buffer is typed"
    (* | File_hash_diff -> "Buffer is typed; file contents changed but AST did not" *)
    (* | Cmis_cached -> "Buffer is not typed; cmis are cached" *)
    | No_cache -> "Buffer is not typed; cmis are not cached"

  let uses_server cache =
    match cache with
    (* | Cmis_cached  *)
    (* | File_hash_diff  *)
    | Buffer_typed -> true
    | No_cache -> false

  let print path = function
    (* | Cmis_cached  *)
    (* | File_hash_diff *)
    | Buffer_typed -> Format.sprintf "%s server" (Fpath.to_string path)
    | No_cache -> Format.sprintf "%s single" (Fpath.to_string path)
end

module Path = struct
  type t = Fpath.t

  let yojson_of_t path = `String (Fpath.to_string path)
end

type t = {
  path : Path.t;
  cache_workflow : Cache_workflow.t;
  version : Yojson.Safe.t;
  comment : string option;
}
[@@deriving yojson_of]

let pp ppf merlin =
  Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t merlin))

let is_server merlin = Cache_workflow.uses_server merlin.cache_workflow

let basic_cmd ppf { path; cache_workflow; _ } =
  Format.fprintf ppf "%s" (Cache_workflow.print path cache_workflow)

let untimed_query_generic ~f cmd =
  let ic = Unix.open_process_in cmd in
  match f ic with
  | res -> (
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> res
      | Unix.WEXITED code ->
          Format.eprintf "merlin exited with code %s\n%!" (string_of_int code);
          exit 1
      | _ ->
          Format.eprintf "merlin closed unexpectedly\n%!";
          exit 2)
  | exception e ->
      ignore (Unix.close_process_in ic);
      let s =
        Format.sprintf "exception while running [%s]: %s\n%!" cmd
          (Printexc.to_string e)
      in
      failwith s

let untimed_query_json cmd =
  let f = Yojson.Safe.from_channel in
  untimed_query_generic ~f cmd

let untimed_query_str cmd =
  let f = input_line in
  untimed_query_generic ~f cmd

let make ?comment path cache_workflow =
  let cmd =
    Printf.sprintf "%s -version" (Cache_workflow.print path cache_workflow)
  in
  let version = `String (untimed_query_str cmd) in
  { path; cache_workflow; version; comment }

module Query_type = struct
  type t =
    | Case_analysis
    | Type_enclosing
    | Occurrences
    | Complete_prefix
    | Expand_prefix
    | Locate
    | Errors
  [@@deriving yojson_of, enumerate]

  let to_string = function
    | Locate -> "locate"
    | Case_analysis -> "case-analysis"
    | Type_enclosing -> "type-enclosing"
    | Occurrences -> "occurrences"
    | Complete_prefix -> "complete-prefix"
    | Expand_prefix -> "expand-prefix"
    | Errors -> "errors"

  type node =
    | Longident
    | Expression
    | Var_pattern
    | Module_expr
    | Module_decl
    | Module_type_decl
  [@@deriving yojson]

  let is_global = function
    | Locate | Case_analysis | Type_enclosing | Occurrences | Complete_prefix
    | Expand_prefix ->
        false
    | Errors -> true

  let has_target qt target =
    let target_nodes =
      match qt with
      | Locate -> [ Longident ]
      | Case_analysis -> [ Expression; Var_pattern ]
      | Type_enclosing ->
          [ Expression; Module_expr; Module_decl; Module_type_decl ]
      | Occurrences -> [ Longident ]
      | Complete_prefix -> [ Longident ]
      | Expand_prefix -> [ Longident ]
      | Errors ->
          (* ToDo: this could by type-safer!! *)
          []
    in
    List.mem target target_nodes
end

module Response = struct
  type t = Yojson.Safe.t
  type value_class = Msg of string | Empty | Other [@@deriving yojson_of]

  type return_class = Return of value_class | Failure | Error | Exception
  [@@deriving yojson_of]

  let yojson_of_t x = x

  let crop_arbitrary_keys keys = function
    | `Assoc answer ->
        let cropped_answer =
          List.fold_left
            (fun answer key -> List.remove_assoc key answer)
            answer keys
        in
        `Assoc cropped_answer
    | _ ->
        (* Fixme *)
        failwith
          "Error while cropping merlin response: reponse should be an \
           association list."

  let get_timing = function
    | `Assoc answer -> (
        match List.assoc "timing" answer with
        | `Assoc timing -> (
            match List.assoc "clock" timing with
            | `Int time -> time
            | _ -> failwith "merlin gave bad output")
        | _ -> failwith "merlin gave bad output")
    | _ -> failwith "merlin gave bad output"

  let strip_file = function
    | `Assoc l ->
        let map_filename = function
          | "file", `String f ->
              let base = Stdlib.Filename.basename f in
              ("file", `String base)
          | other -> other
        in
        let map_value_assoc = function
          | "value", `Assoc v -> ("value", `Assoc (List.map map_filename v))
          | other -> other
        in
        `Assoc (List.map map_value_assoc l)
    | other -> other

  let get_return_class = function
    | `Assoc answer -> (
        match List.assoc "class" answer with
        | `String "return" -> (
            match List.assoc "value" answer with
            | `List [] -> Ok (Return Empty)
            | `String msg -> Ok (Return (Msg msg))
            | _ -> Ok (Return Other))
        | `String "failure" -> Ok Failure
        | `String "error" -> Ok Error
        | `String "exception" -> Ok Exception
        | _ -> Error (Logs.Error "Unknown return class"))
    | _ ->
        Error
          (Logs.Error
             "Error while extracting return classe: Response should have key \
              called class")

  let obfuscate_location subject =
    let pattern = Str.regexp "line [0-9]+, characters [0-9]+-[0-9]+" in
    Str.global_replace pattern "line **, characters **-**" subject

  let strip_location = function
    | `Assoc answer -> (
        match List.assoc "class" answer with
        | `String "exception" ->
            let new_answer =
              List.map
                (function
                  | "value", `String stacktrace ->
                      ("value", `String (obfuscate_location stacktrace))
                  | x -> x)
                answer
            in
            `Assoc new_answer
        | _ -> `Assoc answer)
    | _ ->
        (* Fixme *)
        failwith
          "Error while cropping merlin response: reponse should be an \
           association list."

  let get_query_num = function
    | `Assoc answer -> (
        match List.assoc_opt "query_num" answer with
        | Some (`Int num) -> Ok num
        | _ -> Error (Logs.Error "Unknown query_num type"))
    | _ ->
        Error
          (Logs.Error
             "Error while extracting query num: Response should have key \
              called query_num")
end

module Cmd = struct
  type t = string

  let yojson_of_t cmd = `String cmd

  (* FIXME: instead of having loc optional, it would be nice to have a logic containing: if query_type is local, there's a loc; if it's global, there's none *)
  let make ~query_type ~file ?li ?loc merlin =
    let retrieve_loc ~query_type = function
      | Some loc -> Ok loc
      | None ->
          Error
            (Logs.Error
               (Format.sprintf "Didn't receive a location for %s"
                  (Query_type.to_string query_type)))
    in
    let open Result.Syntax in
    let* query_cmd =
      match query_type with
      | Query_type.Locate ->
          let+ loc = retrieve_loc ~query_type loc in
          Format.asprintf " %a %s -look-for ml -position '%a' -filename %a < %a"
            basic_cmd merlin
            (Query_type.to_string query_type)
            (Location.print_edge Right)
            loc File.pp file File.pp file
      | Case_analysis ->
          let+ loc = retrieve_loc ~query_type loc in
          Format.asprintf "%a %s -start '%a' -end '%a' -filename %a < %a"
            basic_cmd merlin
            (Query_type.to_string query_type)
            (Location.print_edge Left) loc
            (Location.print_edge Right)
            loc File.pp file File.pp file
      | Type_enclosing ->
          let+ loc = retrieve_loc ~query_type loc in
          Format.asprintf "%a %s -position '%a' -index 0 -filename %a < %a"
            basic_cmd merlin
            (Query_type.to_string query_type)
            (Location.print_edge Right)
            loc File.pp file File.pp file
      | Occurrences ->
          let+ loc = retrieve_loc ~query_type loc in
          Format.asprintf "%a %s -identifier-at '%a' -filename %a < %a"
            basic_cmd merlin
            (Query_type.to_string query_type)
            (Location.print_edge Right)
            loc File.pp file File.pp file
      | Complete_prefix | Expand_prefix -> (
          (* TODO: for expand-prefix, it might be interesting to modify the source code to introduce an error *)
          match li with
          | Some li ->
              let+ loc = retrieve_loc ~query_type loc in
              let first_half s = String.(sub s 0 ((length s / 2) + 1)) in
              Format.asprintf
                "%a %s -prefix '%s' -position '%a' -filename %a < %a" basic_cmd
                merlin
                (Query_type.to_string query_type)
                (first_half @@ Longident.name li)
                (Location.print_edge Right)
                loc File.pp file File.pp file
          | None ->
              Error
                (Logs.Error
                   (Format.sprintf
                      "The sampling workflow didn't collect info about the \
                       longident for %s"
                      (Query_type.to_string query_type))))
      | Errors ->
          Result.ok
          @@ Format.asprintf "%a %s -filename %a < %a" basic_cmd merlin
               (Query_type.to_string query_type)
               File.pp file File.pp file
    in
    let cmd =
      match merlin.cache_workflow with
      (* | Cache.File_hash_diff -> *)
      (* TODO: clean up afterwards! *)
      (* Format.asprintf "echo '' >> %a && %s" File.pp file query_cmd *)
      (* | Cmis_cached ->  *)
      (* Format.asprintf "sed -i '1s/^/let () = ()\n/' %a " File.pp file *)
      (* Format.asprintf "echo -e 'let () = ()\n$(cat %a)' > %a " File.pp file File.pp file *)

      (* query_cmd FIXME!!! *)
      | Buffer_typed | No_cache -> query_cmd
    in
    Result.ok cmd

  let some_global_cmd file merlin =
    Format.asprintf "%a errors -filename %a < %a" basic_cmd merlin File.pp file
      File.pp file

  let run_once cmd =
    (* FIXME: it would be better to catch the exception here. *)
    let repl = untimed_query_json cmd in
    repl

  let run ~repeats cmd =
    try
      let rec loop responses = function
        | 0 -> responses
        | n ->
            let new_resp = run_once cmd in
            loop (new_resp :: responses) (n - 1)
      in
      Ok (loop [] repeats)
    with exc -> Error (Logs.Error (Printexc.to_string exc))
end

let init_cache file merlin =
  let cmd = Cmd.some_global_cmd file merlin in
  try
    let _ = Cmd.run_once cmd in
    Ok ()
  with exc -> Error (Logs.Error (Printexc.to_string exc))

let stop_server { path; _ } =
  let command = Fpath.to_string path ^ " server stop-server" in
  match Sys.command command with
  | 255 -> ()
  | code -> failwith ("merlin exited with code " ^ string_of_int code)
