(lang dune 2.9)

(generate_opam_files true)
(source (github pdarragh/camlrack))
(license MIT)
(authors "Pierce Darragh <pierce.darragh@gmail.com>")
(maintainers "Pierce Darragh <pierce.darragh@gmail.com>")
(homepage "https://github.com/pdarragh/camlrack")
(documentation "https://github.com/pdarragh/camlrack")

(package
 (name camlrack)
 (version 0.1.0)
 (synopsis "S-Expression parsing for OCaml")
 (description "'Camlrack' provides the ability to parse strings into
S-expressions and vice versa. It is intended to be simple and minimal, yet
fully-featured.")
 (depends (ocaml (>= 4.12)))
 (tags ("S-expressions" "parsing")))

(package
 (name ppx_camlrack)
 (version 0.0.1)
 (synopsis "PPX for matching S-Expressions")
 (description "'PPX-Camlrack' provides an augmented match expression that
transforms written S-expressions into their Camlrack-compatible types. This
allows for easily working with S-expressions without extra typing.")
 (depends
  (ocaml (>= 4.12))
  (ppxlib (>= 0.23.0))
  (camlrack (>= 0.1.0))))
