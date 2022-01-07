open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  let sexp =
    Camlrack.SExp
      [Camlrack.Symbol "lambda";
      Camlrack.SExp [Camlrack.Symbol "x"; Camlrack.Symbol "y"];
      Camlrack.SExp
        [Camlrack.Symbol "+"; Camlrack.Symbol "x"; Camlrack.Symbol "y"]] in
  if
    Camlrack.Match.sexp_match
      (Camlrack.Match.SPat
         [Camlrack.Match.Symbol "lambda";
         Camlrack.Match.SPat
           [Camlrack.Match.SYMBOL; Camlrack.Match.Symbol "..."];
         Camlrack.Match.ANY;
         Camlrack.Match.Symbol "..."]) sexp
  then sexp
  else
    if
      Camlrack.Match.sexp_match
        (Camlrack.Match.SPat
           [Camlrack.Match.Symbol "+";
           Camlrack.Match.NUMBER;
           Camlrack.Match.NUMBER]) sexp
    then sexp
    else sexp
