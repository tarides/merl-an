  $ INSIDE_DUNE= dune build @check 2>/dev/null || true
  $ merl-an -r 1 -s 1 -p a.ml -q complete-prefix --data=test-data 
  $ jq '.' test-data/logs.json
  [
    "Error",
    "Fatal error: exception Invalid_argument(\"Ppxlib.Longident.parse(empty string): \\\"\\\"\")\n while running `ocamlmerlin server complete-prefix -prefix 'in' -position '1:17'  -filename a.ml < a.ml`"
  ]
