open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  match%spat "(lambda (x y) (+ x y))" with
  | "{lambda {SYMBOL ...} ANY ...}" -> sexp
  | "{+ NUMBER NUMBER}" -> sexp
  | _ -> sexp
