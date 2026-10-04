open! Import

type t = { is_tty : bool; total_files : int }

let create ?is_tty ~total_files () =
  let is_tty =
    match is_tty with Some b -> b | None -> Unix.isatty Unix.stderr
  in
  { is_tty; total_files }

let files_left_str files_left total_files =
  Format.sprintf "%d/%d files left" files_left total_files

let queries_completed_str queries =
  if queries = 1 then "1 query completed"
  else Format.sprintf "%d queries completed" queries

let queries_str queries =
  if queries = 1 then "1 query" else Format.sprintf "%d queries" queries

let update t ~file_idx ~file ~queries =
  if t.is_tty then
    Format.eprintf "\r\027[K[%d/%d files] Analyzing %a: %s...%!" file_idx
      t.total_files File.pp file
      (queries_completed_str queries)

let finish_file t ~file ~queries ~files_left =
  let msg =
    Format.asprintf "Finished %s in %a (%s)" (queries_str queries) File.pp file
      (files_left_str files_left t.total_files)
  in
  if t.is_tty then Format.eprintf "\r\027[K%s\n%!" msg
