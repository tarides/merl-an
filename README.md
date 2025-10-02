# Merl-an

> Tool for [merlin](https://github.com/ocaml/merlin) performance analysis

Merl-an generates queries to analyze [merlin](https://github.com/ocaml/merlin)
performance and behavior, executes them and gathers the results.

## Local development

Since **merl-an** streamlines Merlin's main trunk, we recommend building a local
switch and manually adding the dependency to Merlin:

```shell
opam update
opam switch create . --deps-only --with-doc --with-test --with-dev-setup -y
eval $(opam env)
dune build
```

