open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  match%spat "(lambda (x y) (+ x y))" with
  | "{INTEGER ...}" -> "INTEGER LIST"
  | "{lambda {SYMBOL ...} ANY ...}" -> "LAMBDA EXPRESSION"
  | _ -> "OTHER"
