open! Import

type t = {
  is_tty : bool;
  total_files : int;
  mutable current_file_idx : int;
  mutable current_file : File.t option;
  mutable queries_in_file : int;
}

let create ?is_tty ~total_files () =
  let is_tty =
    match is_tty with Some b -> b | None -> Unix.isatty Unix.stderr
  in
  {
    is_tty;
    total_files;
    current_file_idx = 0;
    current_file = None;
    queries_in_file = 0;
  }

let files_left_str files_left total_files =
  match files_left with
  | 1 -> Format.sprintf "1/%d file left" total_files
  | n -> Format.sprintf "%d/%d files left" n total_files

let queries_str queries =
  match queries with 1 -> "1 query" | n -> Format.sprintf "%d queries" n

let queries_completed_str queries =
  Format.sprintf "%s completed" (queries_str queries)

let start_file t file =
  t.current_file_idx <- t.current_file_idx + 1;
  t.current_file <- Some file;
  t.queries_in_file <- 0;
  if t.is_tty then
    Format.eprintf "\r\027[K[%d/%d files] Analyzing %a...%!" t.current_file_idx
      t.total_files File.pp file

let query_completed t =
  t.queries_in_file <- t.queries_in_file + 1;
  if t.is_tty then
    match t.current_file with
    | Some file ->
        Format.eprintf "\r\027[K[%d/%d files] Analyzing %a: %s...%!"
          t.current_file_idx t.total_files File.pp file
          (queries_completed_str t.queries_in_file)
    | None -> ()

let wrap_write t write sample =
  write sample;
  query_completed t

let finish_file t =
  if t.is_tty then
    match t.current_file with
    | Some file ->
        let files_left = t.total_files - t.current_file_idx in
        Format.eprintf "\r\027[KFinished %s in %a (%s)\n%!"
          (queries_str t.queries_in_file)
          File.pp file
          (files_left_str files_left t.total_files)
    | None -> ()
