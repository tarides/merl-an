open! Import

(* this follows the optimal algorithm for reservoir sampling: https://en.wikipedia.org/wiki/Reservoir_sampling *)

type state = { i : int; w : float }

type 'a t = {
  desired_size : int;
  reservoir : 'a Array.t;
  mutable state : state;
  mutable update_index : int;
}

let init_state ~random_state k =
  let w =
    let r = Random.State.float random_state 1.0 in
    exp (log r /. float_of_int k)
  in
  let i =
    let r = Random.State.float random_state 1.0 in
    k + int_of_float (log r /. log (1. -. w)) + 1
  in
  { i; w }

module Random_state = struct
  type t = Random.State.t

  let make str =
    let char_size = 8 in
    let int_chars = (Sys.int_size + (char_size - 1)) / char_size in
    let length = String.length str in
    let size = (length + (int_chars - 1)) / int_chars in
    let seed = Array.make size 0 in
    for i = 0 to size - 1 do
      let int = ref 0 in
      for j = 0 to int_chars - 1 do
        let index = (i * int_chars) + j in
        if index < length then
          let code = Char.code str.[index] in
          let shift = j * char_size in
          int := !int lor (code lsl shift)
      done;
      seed.(i) <- !int
    done;
    Random.State.make seed
end

let init ~placeholder ~random_state k =
  let reservoir = Array.make k placeholder in
  let state = init_state ~random_state k in
  { reservoir; desired_size = k; state; update_index = 0 }

let update ~random_state smth input =
  if smth.update_index < smth.desired_size then
    let () = smth.reservoir.(smth.update_index) <- input in
    smth.update_index <- smth.update_index + 1
  else if smth.update_index = smth.state.i then
    let random_index = Random.State.int random_state smth.desired_size in
    let () = smth.reservoir.(random_index) <- input in
    let r () = Random.State.float random_state 1.0 in
    let new_i =
      smth.state.i + int_of_float (log (r ()) /. log (1. -. smth.state.w)) + 1
    in
    let new_w =
      smth.state.w *. exp (log (r ()) /. float_of_int smth.desired_size)
    in
    let new_state = { i = new_i; w = new_w } in
    let () = smth.state <- new_state in
    smth.update_index <- smth.update_index + 1
  else smth.update_index <- smth.update_index + 1

let nth n r = r.(n)

let get_samples ~make_sample ~id_counter smth =
  let size = Int.min smth.desired_size smth.update_index in
  List.init size (fun i ->
      make_sample ~id:(i + id_counter) (nth i smth.reservoir))
