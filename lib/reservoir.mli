type 'a t
(** A reservoir sampling instance of samples of type ['a] *)

module Random_state : sig
  type t
  (** A random state, which allows deterministic random operations. *)

  val make : string -> t
  (** Creates a random state from a file name. The state is needed during the
      reservoir update process *)
end

val init : placeholder:'a -> random_state:Random_state.t -> int -> 'a t
(** [init ~placeholder ~random_state size] inits a sampling process to produce a
    sample set of size [size] and type ['a]. [placeholder] can be any value of
    type ['a].*)

val update : random_state:Random_state.t -> 'a t -> 'a -> unit
(** Performs one iteration of the reservoir sampling algorithm *)

val get_samples :
  make_sample:(id:int -> 'a -> 'b) -> id_counter:int -> 'a t -> 'b list
(** Turns a value of type [t] into an enumerated sample set, whose enumeration
    starts at [id_counter] *)
