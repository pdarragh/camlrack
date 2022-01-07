open Camlrack.Match
let test_match (_ : sexp) (_ : sexp_pattern) =
  let sexp = Camlrack.sexp_of_string_exn "(lambda (x y) (+ x y))" in
  if
    Camlrack.Match.sexp_match
      (Camlrack.Match.SPat
         [Camlrack.Match.Symbol "lambda";
         Camlrack.Match.SPat
           [Camlrack.Match.SYMBOL; Camlrack.Match.Symbol "..."];
         Camlrack.Match.ANY;
         Camlrack.Match.Symbol "..."]) sexp
  then sexp
  else sexp
