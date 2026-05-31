(lang dune 3.13)

(name transit)

(generate_opam_files true)

(package
 (name transit)
 (synopsis "Transit JSON reader and writer for OCaml")
 (depends
  (ocaml (>= 5.0))
  yojson))
