(lang dune 3.20)

(name olox)
(version a0ddda4-dirty)

(generate_opam_files true)

(source
 (github FelixTCP/OLox))

(authors "Felix")

(maintainers "Felix")

(license GPL-3.0)

(documentation https://github.com/FelixTCP/OLox/blob/main/README.md)

(package
 (name olox)
 (synopsis "OCaml based Interpreter for the Lox Language")
 (description "An OCaml based Interpreter for the Lox Language - inspired by the book 'crafting interpreters'")
 (depends ocaml)
 (tags
  ("lox" "interpreter" "learning")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
