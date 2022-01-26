open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  let sexp =
    Camlrack.SExp
      [Camlrack.Symbol "lambda";
      Camlrack.SExp [Camlrack.Symbol "x"; Camlrack.Symbol "y"];
      Camlrack.SExp
        [Camlrack.Symbol "+"; Camlrack.Symbol "x"; Camlrack.Symbol "y"]] in
  match sexp with
  | Camlrack.Integer 42 -> string_of_int (sexp_to_int sexp)
  | Camlrack.Integer _ -> string_of_int (sexp_to_int sexp)
  | subgroup1 when
      Camlrack.sexp_match
        (Camlrack.SPat [Camlrack.INTEGER; Camlrack.PSymbol "..."]) subgroup1
      ->
      let subexps = sexp_to_list sexp in
      let ints = List.map sexp_to_int subexps in
      let strings = List.map string_of_int ints in String.concat ", " strings
  | _ -> "OTHER"
