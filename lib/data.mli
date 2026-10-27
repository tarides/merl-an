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

type t
(** The streaming channels and configuration used to collect and write analysis
    data. *)

val init :
  backend:backend ->
  force_yes:bool ->
  Merlin.t ->
  Fpath.t ->
  (t, Rresult.R.msg) result
(** [init ~backend ~force_yes merlin dir_path] returns a data instance for the
    chosen [backend] with initialized streaming channels. The provided path
    [dir_path] is the path of the directory, inside which the data will be
    persisted as json-line-files. Returns an error if [dir_path] already exists
    and the user decides to abort. *)

val init_cache : t -> bool
(** Returns whether the cache should be initialized for this backend. *)

val write : t -> sample -> unit
(** Write analysis data of one sample to the streaming channels. *)

val persist_logs : log:Logs.t -> t -> unit
(** Append the log to the logs output stream. *)

val wrap_up : t -> proj_paths:Fpath.t list -> unit
(** Call this before ending the program. Depending on the backend kind, it
    generates and writes metadata or summaries and closes open streams. *)
