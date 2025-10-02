open! Import

type t = Error of string | Warning of string | Log of string
[@@deriving yojson_of]

let pp ppf data =
  Format.fprintf ppf "%s%!" (Yojson.Safe.to_string (yojson_of_t data))
