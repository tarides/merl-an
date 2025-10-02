include module type of Ppxlib

module Result : sig
  include module type of Stdlib.Result

  module Syntax : sig
    val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
    val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  end
end

module List : sig
  include module type of Stdlib.List

  val fold_over_product :
    l1:'a t -> l2:'b t -> init:'c -> ('c -> 'a * 'b -> 'c) -> 'c
  (** The outer traversal is done over [l1], the inner traversal over [l2]. *)

  val is_empty : 'a t -> bool
end

module Location : sig
  include module type of Ppxlib.Location

  type edge = Left | Right

  val print_edge : edge -> Format.formatter -> t -> unit
  val yojson_of_t : t -> Yojson.Safe.t
end

module Yojson : sig
  module Safe : sig
    include module type of Yojson.Safe

    val yojson_of_t : t -> t
  end
end

include module type of Ppx_yojson_conv_lib.Yojson_conv.Primitives
