# Merl-an

> Tool for [merlin](https://github.com/ocaml/merlin) performance analysis.

Merl-an generates queries to analyze [merlin](https://github.com/ocaml/merlin)
performance and behavior, executes them and gathers the results.

> For each ml and/or mli file in the project, a random (but deterministic) sample set of locations is generated. The different merlin queries are then run on those samples. The analysis results are dumped into json-line files.

## How to use merl-an

> **Important**: Always ensure to build your target project first (using the same OCaml compiler version) before running merl-an against it, as merl-an requires the build artifacts (such as `.cmi` and `.cmt` files).

### Merl-an modes

Merl-an has 3 main usage modes: for performance analysis, for behavioral analysis, and for benchmarking.

#### a. Performance

We use this mode to measure latency, query execution times, and cache hit/miss rates across queries. It preserves detailed timing components and allows running repeated queries on each sample to measure the variance.

**Note**: `performance` is the default command merl-an will run if you don't specify another.

- Example

```sh
merl-an performance --project src/ --queries type-enclosing --per-file-samples 5 --repeats 3
```

#### b. Behavior

We use this mode to test whether changes to merlin modify its responses. It is a very useful way to check for regressions between two merlin versions.

- Example

Assuming we have two merlin branches; A and B,

```sh
merl-an behavior --merlin=/path/to/ocamlmerlin-branch-A --project /path/to/project --data data-A
merl-an behavior --merlin=/path/to/ocamlmerlin-branch-B --project /path/to/project --data data-B
```

With both outputs, you can then diff them to see if there are any changes:

```sh
diff -u data-A/distilled_data.json data-B/distilled_data.json
```

#### c. Benchmarking

Used for benchmarking. It runs queries and aggregates the results into a benchmark summary.

- Example

```shell
merl-an benchmark --project src/ --per-file-samples 5 --repeats 5 --data output
```

### Merl-an inputs

To best use merl-an, it's important to know what it expects as input:

Some of the most common flags you will use with merl-an include:
- `-p, --project`: the path to the project directory or files to analyze. The default is the current working directory.
- `-m, --merlin`: the path to the `ocamlmerlin` binary.
- `--data`: the directory where merl-an's output files will be stored.
- `-s, --per-file-samples`: the number of sample locations to query per file. By default, this value is 10.
- `-q, --queries`: if you would like to run specific query types, you can pass them here comma-separated. The default runs all the query types. Supported query types are: `case-analysis`, `type-enclosing`, `occurrences`, `complete-prefix`, `expand-prefix`, `locate`, and `errors`.
- `-r, --repeats`: this is how many times to repeat each query per sample to measure variance. The default value is 10.
- `--cache`: the caching mechanism to simulate, such as `full-cache` or `no-cache`.
- `--dump-commands`: this flag let's merl-an write all the generated queries to a text file without running them.

**Note**: for all flags checkout the manpage: `merl-an --help`.

### Caching modes

The `--cache` flag simulates different cache mechanisms:

- `full-cache` (*default*): warms the caches before measuring. This is done by running a preliminary query such as errors to ensure the buffer is parsed and typed.
- `no-cache`: simulates a cold start where a file is opened and queried for the first time without cached data.

### Merl-an outputs

The results of merl-an are organized inside the following files: 

- `commands.json`: this contains the exact `ocamlmerlin` commands executed for each query.
- `query_responses.json`: this contains the responses returned by merlin for each query sample.
- `distilled_data.json` (*behavior only*): distilled summary of responses for easy diffing.
- `performances.json` (*performance only*): latency timings, sample IDs, query types, and source locations.
- `bench.json` (*benchmarking only*): structured timing metrics.
- `metadata.json`: metadata.
- `logs.json`: warnings or errors encountered during the run.

## Local development

Since **merl-an** streamlines Merlin's main trunk, we recommend building a local
switch and manually adding the dependency to Merlin:

```shell
opam update
opam switch create . --deps-only --with-doc --with-test --with-dev-setup -y
eval $(opam env)
dune build
```

## Contributing

Please feel free to open issues if you find some buggy behaviour and if you can code, you are also welcomed to submit PRs. Big changes should be discussed in an issue first.
