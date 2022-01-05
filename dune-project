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
