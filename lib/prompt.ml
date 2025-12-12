type default = Yes | No

let ask_yn ~question ~default ~force_yes =
  if force_yes then true
  else
    let rec loop () =
      let prompt =
        match default with
        | Yes -> Printf.sprintf "%s [Y/n] " question
        | No -> Printf.sprintf "%s [y/N] " question
      in
      print_string prompt;
      flush stdout;

      match input_line stdin |> String.trim |> String.lowercase_ascii with
      | "" -> ( match default with Yes -> true | No -> false)
      | "y" | "yes" -> true
      | "n" | "no" -> false
      | _ ->
          print_endline "Please answer yes or no.";
          loop ()
    in
    loop ()
