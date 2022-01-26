open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  match%spat "(lambda (x y) (+ x y))" with
  | "42" -> string_of_int (sexp_to_int sexp)
  | "INTEGER" -> string_of_int (sexp_to_int sexp)
  | "{INTEGER ...}" ->
    let subexps = sexp_to_list sexp in
    let ints = List.map sexp_to_int subexps in
    let strings = List.map string_of_int ints in
    String.concat ", " strings
  | _ -> "OTHER"
