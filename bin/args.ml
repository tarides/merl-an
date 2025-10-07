open Cmdliner

let named f = Term.(app (const f))

let merlin =
  let doc = "Path to the ocamlmerlin executable. Defaults to [ocamlmerlin]." in
  named
    (fun x -> `Merlin x)
    Arg.(value & opt string "ocamlmerlin" & info [ "merlin"; "m" ] ~doc)

let proj_dirs =
  let doc =
    "Directory path(s) of the project(s)/file(s), on which you want merlin to \
     be analyzed. Defaults to the cwd."
  in
  named
    (fun x -> `Proj_dirs x)
    Arg.(value & opt (list string) [ "." ] & info [ "project"; "p" ] ~doc)

let dir_name =
  let doc =
    "Directory, in which the data is going to be stored. If the directory \
     doesn't exist, it's created. If the same directory was already used in \
     the past, the data will be overridden. Defaults to \
     [data/<project>+<unix_timestamp>/]."
  in
  named
    (fun x -> `Dir_name x)
    Arg.(value & opt (some string) None & info [ "data" ] ~doc)

let cache_workflow =
  let doc =
    "This tool supports different workflows simulating different states of the \
     [ocamlmerlin] cache. Wich the option [full-cache], both the cache of the \
     current typing as well as the cmi-cache are warm; the cmt-cache is \
     unpredictable. The option [no-cache] simulates the situation of opening a \
     new project and running a merlin query for the very first time: it uses \
     the single frontend. The option [cmi-cached] is in between: the cmi-cache \
     is warm, the cache of the current typing disabled."
    (*TODO: Add: The option [hot] simulates the situation of having a \
      fully initialized cache: it uses the merlin server frontend and \
      initializes the cache 100%.*)
  in
  let e =
    Arg.enum
    @@ List.map
         (fun qt -> (Merl_an.Merlin.Cache_workflow.to_string qt, qt))
         Merl_an.Merlin.Cache_workflow.all
  in
  named
    (fun x -> `Cache x)
    Arg.(
      value
      & opt e Merl_an.Merlin.Cache_workflow.Full_cache
      & info [ "cache" ] ~doc)

let sample_size =
  (* FIXME: Make that a relative numer: relative to the size of the file. *)
  let doc = "Number of samples per file. Defaults to 30." in
  named
    (fun x -> `Sample_size x)
    Arg.(value & opt int 30 & info [ "sample-size"; "s" ] ~doc)

let query_types =
  let doc =
    let all_query_types =
      List.map Merl_an.Merlin.Query_type.to_string Merl_an.Merlin.Query_type.all
      |> String.concat ", "
    in
    "List of merlin commands you want to be analyzed. Options are "
    ^ all_query_types ^ ". Defaults to all of them."
  in
  let e =
    Arg.enum
    @@ List.map
         (fun qt -> (Merl_an.Merlin.Query_type.to_string qt, qt))
         Merl_an.Merlin.Query_type.all
  in
  named
    (fun x -> `Query_types x)
    Arg.(
      value
      & opt (list e) Merl_an.Merlin.Query_type.all
      & info [ "queries"; "q" ] ~doc)

let extensions =
  let doc =
    "List of file extensions you want this tool to analyze data for. Options \
     are [ml] and [mli]. Defaults to both."
  in
  let e = Arg.enum [ ("ml", "ml"); ("mli", "mli") ] in
  named
    (fun x -> `Extensions x)
    Arg.(value & opt (list e) [ "ml"; "mli" ] & info [ "extensions"; "e" ] ~doc)

let repeats_per_sample =
  let doc =
    "Number of times you want the same query to be run on the same sample. The \
     higher that number, the better to analyze variance. Defaults to 10."
  in
  named
    (fun x -> `Repeats x)
    Arg.(value & opt int 10 & info [ "repeats"; "r" ] ~doc)

let no_full =
  let doc = "Don't dump the full Merlin response." in
  named (fun x -> `No_full x) Arg.(value & flag & info [ "no-full" ] ~doc)

let no_distilled_data =
  let doc =
    "Don't dump the distillation of the Merlin responses down into category \
     data, i.e. into return classes and query number."
  in
  named
    (fun x -> `No_distilled_data x)
    Arg.(value & flag & info [ "no-distilled-data" ] ~doc)

let filter_outliers =
  let doc = "Filter outliers from Merlin responses." in
  named
    (fun x -> `Filter_outliers x)
    Arg.(value & flag & info [ "filter-outliers" ] ~doc)
