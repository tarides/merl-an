open! Import

type t

val create : ?is_tty:bool -> total_files:int -> unit -> t
(** [create ?is_tty ~total_files ()] creates a new progress tracker. If [is_tty]
    is not specified, it is detected via [Unix.isatty Unix.stderr]. *)

val update : t -> file_idx:int -> file:File.t -> queries:int -> unit
(** [update t ~file_idx ~file ~queries] prints an in-place progress update when
    running in an interactive terminal. *)

val finish_file : t -> file:File.t -> queries:int -> files_left:int -> unit
(** [finish_file t ~file ~queries ~files_left] prints a completion message for
    [file]. *)
