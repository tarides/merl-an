open! Import

type sample = {
  id : int;
  responses : Merlin.Response.t list;
  cmd : Merlin.Cmd.t;
  file : File.t;
  loc : Location.t;
  query_type : Merlin.Query_type.t;
}

type behavior_config = { full : bool; distilled_data : bool }
type backend = Performance | Behavior of behavior_config | Benchmark

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
  try f open_safe
  with exn ->
    close_channels !opened;
    raise exn

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

  type t1 = { name : string; metrics : Benchmark_metric.t list }
  [@@deriving yojson_of]

  let convert ({ name; metrics } : t) =
    { name; metrics = StringMap.bindings metrics |> List.map snd }
end

module Benchmark_summary = struct
  type t = { mutable results : Benchmark_result.t StringMap.t }
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

  let pp ppf data =
    Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))
end

module Performance = struct
  type t = {
    performances : out_channel;
    query_responses : out_channel;
    commands : out_channel;
    logs : out_channel;
    merlin : Merlin.t;
  }

  let write_analysis_data channels { id; responses; cmd; file; loc; query_type }
      =
    let max_timing, timings, responses = extract_timings responses in
    let perf =
      { P.timings; max_timing; file; query_type; sample_id = id; loc }
    in
    write_json_line channels.performances P.pp perf;
    Query_response.write_stripped channels.query_responses ~id ~cmd ~responses;
    Command.write channels.commands ~id ~cmd

  let persist_logs ~log channels = write_json_line channels.logs Logs.pp log

  let create_initial ~data_dir merlin =
    with_open_channels (fun open_channel ->
        let performances = open_channel data_dir performances_file in
        let query_responses = open_channel data_dir query_responses_file in
        let commands = open_channel data_dir commands_file in
        let logs = open_channel data_dir logs_file in
        { performances; query_responses; commands; logs; merlin })

  module Metadata = struct
    type t = {
      date : string option;
      proj : string list;
      total_time : float;
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
        { date; proj = List.map Fpath.to_string proj_paths; total_time; merlin }
      in
      write_single pp data_dir file_name metadata
  end

  let wrap_up t ~data_dir ~proj_paths ~merlin =
    Fun.protect
      ~finally:(fun () ->
        close_channels [ t.performances; t.query_responses; t.commands; t.logs ])
      (fun () ->
        write_single Merlin.pp data_dir merlin_file merlin;
        Metadata.produce_and_write ~data_dir ~proj_paths ~merlin)

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

module Behavior = struct
  type t = {
    full_responses : out_channel option;
    distilled_data : out_channel option;
    commands : out_channel;
    logs : out_channel;
  }

  let persist_logs ~log channels = write_json_line channels.logs Logs.pp log

  let write_analysis_data channels { id; responses; cmd; _ } =
    Command.write channels.commands ~id ~cmd;
    Option.iter
      (fun oc ->
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
        write_json_line oc Query_response.pp resp)
      channels.full_responses;
    Option.iter
      (fun oc ->
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
                persist_logs ~log channels;
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
                persist_logs ~log channels;
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
                persist_logs ~log:log1 channels;
                persist_logs ~log:log2 channels)
        | _ -> ())
      channels.distilled_data

  let create_initial config ~data_dir =
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
        { full_responses; distilled_data; commands; logs })

  let wrap_up t =
    Option.iter close_out_noerr t.full_responses;
    Option.iter close_out_noerr t.distilled_data;
    close_channels [ t.commands; t.logs ]

  let all_files () =
    [ full_responses_file; distilled_data_file; commands_file; logs_file ]
end

module Benchmark = struct
  type t = {
    bench : Benchmark_summary.t;
    query_responses : out_channel;
    commands : out_channel;
    logs : out_channel;
    merlin : Merlin.t;
  }

  let create_initial ~data_dir merlin =
    with_open_channels (fun open_channel ->
        let query_responses = open_channel data_dir query_responses_file in
        let commands = open_channel data_dir commands_file in
        let logs = open_channel data_dir logs_file in
        {
          bench = { results = StringMap.empty };
          query_responses;
          commands;
          logs;
          merlin;
        })

  let persist_logs ~log channels = write_json_line channels.logs Logs.pp log

  let all_files () =
    [ bench_file; query_responses_file; commands_file; logs_file; merlin_file ]

  let write_analysis_data channels { id; responses; cmd; query_type; _ } =
    let _max_timing, timings, responses = extract_timings responses in
    let metric =
      {
        Benchmark_metric.name = Merlin.Query_type.to_string query_type;
        value = timings;
        units = "ms";
      }
    in
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
        upd channels.bench.results
    in
    channels.bench.results <- result;
    Query_response.write_stripped channels.query_responses ~id ~cmd ~responses;
    Command.write channels.commands ~id ~cmd

  let wrap_up t ~data_dir ~merlin =
    Fun.protect
      ~finally:(fun () ->
        close_channels [ t.query_responses; t.commands; t.logs ])
      (fun () ->
        write_single Benchmark_summary.pp data_dir bench_file t.bench;
        write_single Merlin.pp data_dir merlin_file merlin)
end

let create_dir_recursively ~force_yes data_path =
  let dir = Fpath.to_string data_path in
  let question =
    Format.asprintf
      "Your data directory %a already exists, so the data in there will be \
       overridden. Sure you want to proceed?"
      Fpath.pp data_path
  in
  if
    Sys.file_exists dir && not Prompt.(ask_yn ~question ~default:Yes ~force_yes)
  then Error (`Msg "Operation cancelled.\n")
  else
    let rec get_all_subpaths acc = function
      | _ :: rest as all ->
          let acc = String.concat "/" (List.rev all) :: acc in
          get_all_subpaths acc rest
      | [] -> acc
    in

    Fpath.segs data_path |> List.rev |> get_all_subpaths []
    |> List.iter (fun dir -> try Sys.mkdir dir 0o777 with _ -> ())
    |> Result.ok

let create_files dir files =
  List.iter
    (fun fn ->
      let path = Fpath.(to_string @@ append dir fn) in
      let descr = Unix.openfile path [ Unix.O_CREAT ] 0o777 in
      Unix.close descr)
    files

let some_file_isnt_writable data_path files =
  List.exists
    (fun fn ->
      match open_out Fpath.(to_string @@ append data_path fn) with
      | exception _ -> true
      | oc ->
          close_out_noerr oc;
          false)
    files

type t = {
  init_cache : bool;
  write : sample -> unit;
  persist_logs : log:Logs.t -> unit;
  wrap_up : proj_paths:Fpath.t list -> unit;
}

let init ~backend ~force_yes merlin data_dir =
  let open Result.Syntax in
  let* () = create_dir_recursively ~force_yes data_dir in
  let data_files =
    match backend with
    | Performance -> Performance.all_files ()
    | Behavior _ -> Behavior.all_files ()
    | Benchmark -> Benchmark.all_files ()
  in
  create_files data_dir data_files;
  if some_file_isnt_writable data_dir data_files then (
    Format.eprintf "It's not possible to write to the data files\n%!";
    exit 20)
  else
    let t =
      match backend with
      | Performance ->
          let channels = Performance.create_initial ~data_dir merlin in
          {
            init_cache = Merlin.is_server merlin;
            write = Performance.write_analysis_data channels;
            persist_logs = (fun ~log -> Performance.persist_logs ~log channels);
            wrap_up =
              (fun ~proj_paths ->
                Performance.wrap_up channels ~data_dir ~proj_paths ~merlin);
          }
      | Behavior config ->
          let channels = Behavior.create_initial config ~data_dir in
          {
            init_cache = false;
            write = Behavior.write_analysis_data channels;
            persist_logs = (fun ~log -> Behavior.persist_logs ~log channels);
            wrap_up = (fun ~proj_paths:_ -> Behavior.wrap_up channels);
          }
      | Benchmark ->
          let channels = Benchmark.create_initial ~data_dir merlin in
          {
            init_cache = Merlin.is_server merlin;
            write = Benchmark.write_analysis_data channels;
            persist_logs = (fun ~log -> Benchmark.persist_logs ~log channels);
            wrap_up =
              (fun ~proj_paths:_ ->
                Benchmark.wrap_up channels ~data_dir ~merlin);
          }
    in
    Ok t

let init_cache t = t.init_cache
let write t sample = t.write sample
let persist_logs ~log t = t.persist_logs ~log
let wrap_up t ~proj_paths = t.wrap_up ~proj_paths
