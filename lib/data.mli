open! Import

(* FIXME: this type should be split into several types and renamed *)
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

type t
(** The streaming channels and configuration used to collect and write analysis
    data. *)

(* TODO: this shouldn't be only exactly merlin and data_dir, but all configuration data. and the data should be stored in Data.t as well*)
val with_data :
  backend:backend ->
  force_yes:bool ->
  merlin:Merlin.t ->
  data_dir:Fpath.t ->
  proj_paths:Fpath.t list ->
  (t -> ('a, ([> Rresult.R.msg ] as 'b)) result) ->
  ('a, 'b) result
(** [with_data ~backend ~force_yes ~merlin ~data_dir ~proj_paths f] returns the
    result of running [f] with a data instance and initialized streaming
    channels. Streaming channels are guaranteed to be closed after [f] finishes.
    Returns an error if [data_dir] already exists and the user decides to abort.
*)

val init_cache : t -> bool
(** Returns whether the cache should be initialized for this backend. *)

val write : t -> sample -> unit
(** Write analysis data of one sample to the streaming channels. *)

val persist_logs : log:Logs.t -> t -> unit
(** Append the log to the logs output stream. *)
