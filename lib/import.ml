include Ppxlib

module Result = struct
  include Stdlib.Result

  module Syntax = struct
    let ( let+ ) x f = map f x
    let ( let* ) x f = bind x f
  end
end

module List = struct
  include Stdlib.List

  let fold_over_product ~l1 ~l2 ~init f =
    fold_left
      (fun outer_acc x ->
        fold_left (fun inner_acc y -> f inner_acc (x, y)) outer_acc l2)
      init l1

  let is_empty l = length l = 0
end

module Location = struct
  include Location

  type edge = Left | Right

  let print_edge e ppf loc =
    (* let open Lexing in *)
    let col, line =
      match e with
      | Left ->
          let pos = loc.loc_start in
          (pos.pos_cnum - pos.pos_bol, pos.pos_lnum)
      | Right ->
          let pos = loc.loc_end in
          (pos.pos_cnum - pos.pos_bol - 1, pos.pos_lnum)
    in
    Format.fprintf ppf "%i:%i" line col

  let yojson_of_t loc = `String (Format.asprintf "%a" print loc)
end

module Yojson = struct
  module Safe = struct
    include Yojson.Safe

    let yojson_of_t = Fun.id
  end
end

include Ppx_yojson_conv_lib.Yojson_conv.Primitives
