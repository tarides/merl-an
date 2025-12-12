open! Import

val analyze :
  backend:(module Backend.Data_tables) ->
  repeats:int ->
  cache_workflow:Merlin.Cache_workflow.t ->
  merlin_path:string ->
  proj_dirs:string list ->
  data_dir:string option ->
  per_file_samples:int ->
  total_samples:int option ->
  query_types:Merlin.Query_type.t list ->
  filter_outliers:bool ->
  extensions:string list ->
  force_yes:bool ->
  (unit, Rresult.R.msg) Result.t
