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
         [Camlrack.Match.PSymbol "lambda";
         Camlrack.Match.SPat
           [Camlrack.Match.SYMBOL; Camlrack.Match.PSymbol "..."];
         Camlrack.Match.ANY;
         Camlrack.Match.PSymbol "..."]) sexp
  then sexp
  else
    if
      Camlrack.Match.sexp_match
        (Camlrack.Match.SPat
           [Camlrack.Match.PSymbol "+";
           Camlrack.Match.INTEGER;
           Camlrack.Match.INTEGER]) sexp
    then sexp
    else sexp
