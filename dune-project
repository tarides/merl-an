(lang dune 3.23)

(name merl-an)

(cram enable)

(generate_opam_files true)

(source
 (github pitag-ha/merl-an))

(authors "Sonja Heinze" "Rafał Gwoździński")

(maintainers "Sonja Heinze")

(license MIT)

(package
 (name merl-an)
 (synopsis "Tool for merlin performance analysis")
 (description
  "Merl-an generates queries to analyze merlin performance and behavior, executes them and gathers the results.")
 (depends
  (ocaml
   (>= 5.4))
  ppxlib
  yojson
  fpath
  bos
  cmdliner
  ppx_deriving_yojson
  ppx_enumerate
  ppx_yojson_conv
  ptime
  merlin-lib
  dot-merlin-reader
  (merlin :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (= 0.29.0)
    :with-dev-setup))))
