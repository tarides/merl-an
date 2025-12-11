open! Import

(** The different kinds of data backends that are supported. *)
type kind = Perf | Regr | Error_regr | Bench

module type Data_tables = sig
  type t
  (** The type representing the data the way it will be structured: several
      tables; each of them will be dumped as a json-line file. *)

  val kind : kind
  (** The backend kind *)

  val create_initial : Merlin.t -> t
  (** Initializes the tables. Data can then be appended to them. *)

  val init_cache : t -> bool

  val update_analysis_data :
    id:int ->
    responses:Merlin.Response.t list ->
    cmd:Merlin.Cmd.t ->
    file:File.t ->
    loc:Location.t ->
    query_type:Merlin.Query_type.t ->
    t ->
    unit
  (** Append analyzis data. *)

  val persist_logs : log:Logs.t -> t -> unit
  (** Append logs. *)

  val dump : dump_dir:Fpath.t -> t -> unit
  (** Dump all tables (as json-lines). *)

  val all_files : unit -> Fpath.t list
  (** Returns the list of all files to which the data is dumped with [dump]. *)

  val wrap_up :
    t -> dump_dir:Fpath.t -> proj_paths:Fpath.t list -> merlin:Merlin.t -> unit
  (** Call this, before ending the program. It makes sure there's no data left
      in memory anymore and, in case there still is, dumps it (TODO!). Depending
      on the backend kind, it also generates and dumps some metadata. *)
end

module Performance : Data_tables
(** The backend for analyzing [merlin]'s performance. *)

type behavior_config = { full : bool; distilled_data : bool }

val behavior : behavior_config -> (module Data_tables)
(** The backend for testing possible end-to-end [merlin] regressions. *)

module Benchmark : Data_tables
