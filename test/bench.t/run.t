  $ merl-an benchmark -r 2 -s 2 -p bench.ml,bench1.ml --data=test-data

  $ jq '.results |= map( .metrics |= map(.value |= map(0)))' test-data/bench.json
  {
    "results": [
      {
        "name": "full-cache",
        "metrics": [
          {
            "name": "case-analysis",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "complete-prefix",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "errors",
            "value": [
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "expand-prefix",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "locate",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "occurrences",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          },
          {
            "name": "type-enclosing",
            "value": [
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0
            ],
            "units": "ms"
          }
        ]
      }
    ]
  }

(* FIXME: What's cb-check?)
$ cat test-data/bench.json |
> jq '.results |= map( .metrics |= map(.value |= map(0)))' |
> cb-check
