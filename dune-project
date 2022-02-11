(lang dune 2.9)
(name camlrack)

; The same version number is used for both packages.
(version 0.10.1)

(generate_opam_files true)
(source (github pdarragh/camlrack))
(license MIT)
(authors "Pierce Darragh <pierce.darragh@gmail.com>")
(maintainers "Pierce Darragh <pierce.darragh@gmail.com>")
(homepage "https://github.com/pdarragh/camlrack")
(documentation "https://github.com/pdarragh/camlrack")

(package
 (name camlrack)
 (synopsis "S-Expression parsing for OCaml")
 (description "'Camlrack' provides the ability to parse strings into
S-Expressions and vice versa. It is intended to be simple and minimal, yet
fully-featured.")
 (depends
  (ocaml (>= 4.12))
  (ounit2 (and :with-test (>= 2.2.5))))
 (tags ("S-Expressions" "parsing")))

(package
 (name ppx_camlrack)
 (synopsis "PPX for matching S-Expressions")
 (description "'PPX-Camlrack' provides an augmented match expression that
transforms written S-Expressions into their Camlrack-compatible types. This
allows for easily working with S-Expressions without extra typing.")
 (depends
  (ocaml (>= 4.12))
  (ounit2 (and :with-test (>= 2.2.5)))
  (ppxlib (>= 0.23.0))
  (camlrack (= :version)))
 (tags ("S-Expressions" "parsing" "matching" "ppx" "extension")))
