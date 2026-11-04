open! Import

type kind = Perf | Regr | Error_regr | Bench

module type Data_channels = sig
  type t

  val kind : kind
  val init_cache : t -> bool

  val write_analysis_data :
    id:int ->
    responses:Merlin.Response.t list ->
    cmd:Merlin.Cmd.t ->
    file:File.t ->
    loc:Location.t ->
    query_type:Merlin.Query_type.t ->
    t ->
    unit

  val persist_logs : log:Logs.t -> t -> unit
  val all_files : unit -> Fpath.t list

  val with_channels :
    data_dir:Fpath.t ->
    merlin:Merlin.t ->
    proj_paths:Fpath.t list ->
    (t -> 'a) ->
    'a
end

let performances_file = Fpath.v "performances.json"
let query_responses_file = Fpath.v "query_responses.json"
let commands_file = Fpath.v "commands.json"
let logs_file = Fpath.v "logs.json"
let merlin_file = Fpath.v "merlin.json"
let full_responses_file = Fpath.v "full_responses.json"
let distilled_data_file = Fpath.v "distilled_data.json"
let bench_file = Fpath.v "bench.json"

let write_json_line oc pp data =
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a\n%!" pp data

let open_channel data_dir file_name =
  let file_path = Fpath.(to_string @@ append data_dir file_name) in
  open_out file_path

let close_channels channels = List.iter close_out_noerr channels

let with_open_channels f =
  let opened = ref [] in
  let open_safe data_dir file_name =
    let oc = open_channel data_dir file_name in
    opened := oc :: !opened;
    oc
  in
  Fun.protect
    ~finally:(fun () -> close_channels !opened)
    (fun () -> f open_safe)

let write_single pp data_dir file_name data =
  let oc = open_channel data_dir file_name in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> write_json_line oc pp data)

module P = struct
  type t = {
    sample_id : int;
    timings : int list;
    max_timing : int;
    file : File.t;
    query_type : Merlin.Query_type.t;
    loc : Location.t;
  }
  [@@deriving yojson_of]

  (* FIXME: print each of the sample repeats in a separate json field *)
  let pp ppf data =
    Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))
end

module Query_response = struct
  type t = {
    sample_id : int;
    cmd : Merlin.Cmd.t;
    responses : Merlin.Response.t list;
  }
  [@@deriving yojson_of]

  (* FIXME: print the sample repeats in a separate json field *)
  let pp ppf data =
    Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))

  let write_stripped oc ~id ~cmd ~responses =
    let responses =
      List.map
        (fun resp ->
          Merlin.Response.(
            crop_arbitrary_keys [ "value" ] @@ strip_location @@ resp))
        responses
    in
    write_json_line oc pp { sample_id = id; cmd; responses }
end

module Command = struct
  type t = { sample_id : int; cmd : Merlin.Cmd.t } [@@deriving yojson_of]

  let pp ppf data =
    Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))

  let write oc ~id ~cmd = write_json_line oc pp { sample_id = id; cmd }
end

let extract_timings responses =
  let rec loop ~max_timing ~responses ~timings = function
    | [] -> (max_timing, timings, responses)
    | resp :: rest ->
        let timing = Merlin.Response.get_timing resp in
        let timings = timing :: timings in
        let responses = resp :: responses in
        let max_timing = Int.max timing max_timing in
        loop ~max_timing ~timings ~responses rest
  in
  loop ~max_timing:Int.min_int ~responses:[] ~timings:[] responses

module Benchmark_metric = struct
  type t = { name : string; value : int list; units : string }
  [@@deriving yojson_of]
end

module StringMap = Map.Make (String)

module Benchmark_result = struct
  type t = { name : string; metrics : Benchmark_metric.t StringMap.t }

  let update (result : t) (metric : Benchmark_metric.t) =
    let f x =
      match x with
      | Some (me : Benchmark_metric.t) ->
          Some { me with value = List.append metric.value me.value }
      | None -> Some metric
    in
    { result with metrics = StringMap.update metric.name f result.metrics }

  let create name (metric : Benchmark_metric.t) =
    { name; metrics = StringMap.add metric.name metric StringMap.empty }

  (* TODO: Figure out a way to remove intermediate type *)
  type t1 = { name : string; metrics : Benchmark_metric.t list }
  [@@deriving yojson_of]

  let convert ({ name; metrics } : t) =
    { name; metrics = StringMap.bindings metrics |> List.map snd }
end

module Benchmark_summary = struct
  type t = { mutable results : Benchmark_result.t StringMap.t }

  (* TODO: Figure out a way to remove intermediate type *)
  type t1 = { results : Benchmark_result.t1 list } [@@deriving yojson_of]

  let pp ppf data =
    let convert ({ results } : t) =
      {
        results =
          StringMap.bindings results |> List.map snd
          |> List.map Benchmark_result.convert;
      }
    in
    Format.fprintf ppf "%s%!"
      (Yojson.Safe.to_string (yojson_of_t1 (convert data)))
end

module Distilled_data = struct
  type t = {
    sample_id : int;
    cmd : Merlin.Cmd.t;
    return : Merlin.Response.return_class option;
    query_num : int option;
  }
  [@@deriving yojson_of]

  (* FIXME: print the sample repeats in a separate json field *)
  let pp ppf data =
    Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))
end

(* module Files = struct
     type kind = Ml | Mli
     type t = {
       file_id : string (* could be the hash of the content *)
       name : string option (* None, when using [--sanitize] *)
       len : int;
       functor_depth : int;
       num_first_class_modules : int;
       cmi_deps : t list; (* note: probably not a good idea to have t itself... *)
       cmt_deps : t list; (* note: probably not a good idea to have t itself... *)
       commit_sha : string option;
     }
     let get_commit_sha ~proj_path =
        let cmd = "git rev-parse HEAD" in
        try
          let cwd = Unix.getcwd () in
          Unix.chdir @@ Fpath.to_string proj_path;
          let ic = Unix.open_process_in cmd in
          Unix.chdir cwd;
          match input_line ic with
          | sha -> Ok sha
          | exception exc ->
              let err =
                Logs.Warning
                  (Format.sprintf
                     "Warning: something went wrong trying to get the commit sha \
                      of the source code project: %s"
                     (Printexc.to_string exc))
              in
              Error err
        with exc ->
          let err =
            Logs.Warning
              (Format.sprintf
                 "Warning: something went wrong trying to get the commit sha of \
                  the source code project: %s"
                 (Printexc.to_string exc))
          in
          Error err
   end *)

module Performance = struct
  (* TODO: add more data/restructure data, such as:
     - add a files "table" containing things like
        - the length of the file (size of AST)
        - in-file max functor depth
        - number of first-class modules shipped around in the file
        - number of cmi/cmt dependencies
        - commit sha of that file
     - have a finer-grained level than sample_id to allow dedicated fields for the repeats of one query
     - possibly: add a query_types "table":
        - all query types used in that `merl-an` run
        - the AST node type(s) corresponding to that query
        - the way the cache is being initialized for that query
        - the way the query is run
  *)
  type t = {
    performances : out_channel;
    query_responses : out_channel;
    commands : out_channel;
    logs : out_channel;
    merlin : Merlin.t;
  }

  let init_cache p = Merlin.is_server p.merlin
  let kind = Perf

  let write_analysis_data ~id ~responses ~cmd ~file ~loc ~query_type t =
    let max_timing, timings, responses = extract_timings responses in
    let perf =
      { P.timings; max_timing; file; query_type; sample_id = id; loc }
    in
    write_json_line t.performances P.pp perf;
    Query_response.write_stripped t.query_responses ~id ~cmd ~responses;
    Command.write t.commands ~id ~cmd

  let persist_logs ~log t = write_json_line t.logs Logs.pp log

  module Metadata = struct
    type t = {
      date : string option;
      proj : string list;
      total_time : float; (* query_time : float; *)
      merlin : Merlin.t;
    }
    [@@deriving yojson_of]

    let file_name = Fpath.v "metadata.json"

    let pp ppf data =
      Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))

    let get_date () =
      let epoch = Unix.time () |> Ptime.of_float_s |> Option.get in
      let (year, month, day), _ = Ptime.to_date_time epoch in
      Printf.sprintf "%i/%i/%i" day month year

    let produce_and_write ~data_dir ~proj_paths ~merlin =
      let metadata =
        let total_time = Sys.time () in
        let date = Some (get_date ()) in
        {
          date;
          proj = List.map Fpath.to_string proj_paths;
          total_time;
          merlin;
          (* query_time; *)
        }
      in
      write_single pp data_dir file_name metadata
  end

  let with_channels ~data_dir ~merlin ~proj_paths f =
    with_open_channels (fun open_channel ->
        let performances = open_channel data_dir performances_file in
        let query_responses = open_channel data_dir query_responses_file in
        let commands = open_channel data_dir commands_file in
        let logs = open_channel data_dir logs_file in
        let t = { performances; query_responses; commands; logs; merlin } in
        Fun.protect
          ~finally:(fun () ->
            write_single Merlin.pp data_dir merlin_file merlin;
            Metadata.produce_and_write ~data_dir ~proj_paths ~merlin)
          (fun () -> f t))

  let all_files () =
    [
      Metadata.file_name;
      performances_file;
      query_responses_file;
      commands_file;
      logs_file;
      merlin_file;
    ]
end

type behavior_config = { full : bool; distilled_data : bool }

let behavior config =
  let module Behavior = struct
    type t = {
      full_responses : out_channel option;
      distilled_data : out_channel option;
      commands : out_channel;
      logs : out_channel;
    }

    let kind = Regr
    let persist_logs ~log t = write_json_line t.logs Logs.pp log

    let write_analysis_data ~id ~responses ~cmd ~file:_ ~loc:_ ~query_type:_ t =
      Command.write t.commands ~id ~cmd;
      t.full_responses
      |> Option.iter (fun oc ->
          let responses =
            List.map
              (fun resp ->
                Merlin.Response.(
                  strip_file
                  @@ crop_arbitrary_keys [ "timing"; "cache"; "heap_mbytes" ]
                  @@ strip_location @@ resp))
              responses
          in
          let resp = { Query_response.sample_id = id; cmd; responses } in
          write_json_line oc Query_response.pp resp);
      t.distilled_data
      |> Option.iter (fun oc ->
          match responses with
          | [ resp ] -> (
              match
                ( Merlin.Response.get_return_class resp,
                  Merlin.Response.get_query_num resp )
              with
              | Ok return, Ok query_num ->
                  let new_entry =
                    {
                      Distilled_data.sample_id = id;
                      return = Some return;
                      query_num = Some query_num;
                      cmd;
                    }
                  in
                  write_json_line oc Distilled_data.pp new_entry
              | Error log, Ok query_num ->
                  persist_logs ~log t;
                  let new_entry =
                    {
                      Distilled_data.sample_id = id;
                      return = None;
                      query_num = Some query_num;
                      cmd;
                    }
                  in
                  write_json_line oc Distilled_data.pp new_entry
              | Ok return, Error log ->
                  persist_logs ~log t;
                  let new_entry =
                    {
                      Distilled_data.sample_id = id;
                      return = Some return;
                      query_num = None;
                      cmd;
                    }
                  in
                  write_json_line oc Distilled_data.pp new_entry
              | Error log1, Error log2 ->
                  persist_logs ~log:log1 t;
                  persist_logs ~log:log2 t)
          | _ -> (*FIXME*) ())

    let with_channels ~data_dir ~merlin:_ ~proj_paths:_ f =
      with_open_channels (fun open_channel ->
          let full_responses =
            if config.full then Some (open_channel data_dir full_responses_file)
            else None
          in
          let distilled_data =
            if config.distilled_data then
              Some (open_channel data_dir distilled_data_file)
            else None
          in
          let commands = open_channel data_dir commands_file in
          let logs = open_channel data_dir logs_file in
          let t = { full_responses; distilled_data; commands; logs } in
          f t)

    let init_cache _ = false

    let all_files () =
      [ full_responses_file; distilled_data_file; commands_file; logs_file ]
  end in
  (module Behavior : Data_channels)

module Benchmark = struct
  type t = {
    bench : Benchmark_summary.t;
    query_responses : out_channel;
    commands : out_channel;
    logs : out_channel;
    merlin : Merlin.t;
  }

  let kind = Bench
  let init_cache b = Merlin.is_server b.merlin
  let persist_logs ~log t = write_json_line t.logs Logs.pp log

  let all_files () =
    [ bench_file; query_responses_file; commands_file; logs_file; merlin_file ]

  let write_analysis_data ~id ~responses ~cmd ~file:_file
      ~loc:(_loc : Import.location) ~query_type t =
    let _max_timing, timings, responses = extract_timings responses in
    let metric =
      {
        Benchmark_metric.name = Merlin.Query_type.to_string query_type;
        value = timings;
        units = "ms";
      }
    in
    (* TODO: Pass it instead of hardcoding *)
    let cache_workflow = Merlin.Cache_workflow.Full_cache in
    let upd = function
      | Some x -> Some (Benchmark_result.update x metric)
      | None ->
          Some
            (Benchmark_result.create
               (Merlin.Cache_workflow.to_string cache_workflow)
               metric)
    in
    let result =
      StringMap.update
        (Merlin.Cache_workflow.to_string cache_workflow)
        upd t.bench.results
    in
    t.bench.results <- result;
    Query_response.write_stripped t.query_responses ~id ~cmd ~responses;
    Command.write t.commands ~id ~cmd

  let with_channels ~data_dir ~merlin ~proj_paths:_ f =
    with_open_channels (fun open_channel ->
        let query_responses = open_channel data_dir query_responses_file in
        let commands = open_channel data_dir commands_file in
        let logs = open_channel data_dir logs_file in
        let t =
          {
            bench = { results = StringMap.empty };
            query_responses;
            commands;
            logs;
            merlin;
          }
        in
        Fun.protect
          ~finally:(fun () ->
            write_single Benchmark_summary.pp data_dir bench_file t.bench;
            write_single Merlin.pp data_dir merlin_file merlin)
          (fun () -> f t))
end
