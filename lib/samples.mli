open! Import

type t
(** Represents a sample set on a fixed file for a fixed merlin query type. *)

val generate :
  per_file_samples:int ->
  id_counter:int ->
  File.t ->
  Merlin.Query_type.t ->
  (t * int) option
(** Given a source code file [f] and a merlin query type [qt], we consider all
    locations in the file that the given query type can act on. Let's call the
    set of all those locations [population]. Then,
    [generate ~per_file_samples ~id_counter f qt] generates a sample set of size
    [min per_file_samples <popultion size>] of that population with integer IDs
    starting at [id_counter]. In case of success, it returns the generated
    sample set together with an updated [id_counter]. *)

val analyze :
  init_cache:bool ->
  merlin:Merlin.t ->
  repeats:int ->
  update:(Data.sample -> unit) ->
  filter_outliers:bool ->
  t ->
  (unit, Logs.t) Result.t
(** [analyze ~merlin ~repeats data samples] appends new analysis data to [data].
    The data results from running [merlin] on the [samples] (notice that
    [samples] also contains info on the file and on the query type the samples
    are for); it runs the query [repeats] times. The new data is appended to
    [data] as a side-effect. *)
