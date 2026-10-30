open! Import

type t

val create : ?is_tty:bool -> total_files:int -> unit -> t
(** [create ?is_tty ~total_files ()] creates a new progress tracker. If [is_tty]
    is not specified, it is detected via [Unix.isatty Unix.stderr]. *)

val start_file : t -> File.t -> unit
(** [start_file t file] records that analysis of [file] has begun and prints an
    initial progress indicator if [is_tty] is true. *)

val query_completed : t -> unit
(** [query_completed t] records that a query for the current file has completed
    and updates the in-place progress line. *)

val wrap_write : t -> ('a -> unit) -> 'a -> unit
(** [wrap_write t write] wraps a write function so that [query_completed t] is
    called each time data is written. *)

val finish_file : t -> unit
(** [finish_file t] prints a completion message for the current file. *)
