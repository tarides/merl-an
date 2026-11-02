  $ merl-an behavior \
  > --merlin "merlin_mock stderr" \
  > --cache=no-cache \
  > --project=files/test.ml \
  > --per-file-samples=1 \
  > --data=test-data

  $ cat test-data/logs.json
  ["Warning","Fatal error: exception Invalid_argument(\"Ppxlib.Longident.parse(empty string): \\\"\\\"\")"]
  ["Error","Unknown query_num type"]
  ["Warning","Fatal error: exception Invalid_argument(\"Ppxlib.Longident.parse(empty string): \\\"\\\"\")"]
  ["Error","Unknown query_num type"]
  ["Log","File \"files/test.ml\": there are no samples for query [occurrences]."]
  ["Log","File \"files/test.ml\": there are no samples for query [complete-prefix]."]
  ["Log","File \"files/test.ml\": there are no samples for query [expand-prefix]."]
  ["Log","File \"files/test.ml\": there are no samples for query [locate]."]
  ["Warning","Fatal error: exception Invalid_argument(\"Ppxlib.Longident.parse(empty string): \\\"\\\"\")"]
  ["Error","Unknown query_num type"]
