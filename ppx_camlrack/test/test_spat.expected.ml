open Camlrack
let test_match (_ : sexp) (_ : sexp_pattern) =
  let sexp =
    Camlrack.SExp
      [Camlrack.Symbol "lambda";
      Camlrack.SExp [Camlrack.Symbol "x"; Camlrack.Symbol "y"];
      Camlrack.SExp
        [Camlrack.Symbol "+"; Camlrack.Symbol "x"; Camlrack.Symbol "y"]] in
  match sexp with
  | Camlrack.SExp [] -> "INTEGER LIST"
  | Camlrack.SExp ((Camlrack.Integer _)::[]) -> "INTEGER LIST"
  | Camlrack.SExp ((Camlrack.Integer _)::(Camlrack.Integer _)::[]) ->
      "INTEGER LIST"
  | Camlrack.SExp ((Camlrack.SExp subgroup1)::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.INTEGER) subgroup1 ->
      "INTEGER LIST"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp [])::[]) ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::[]))::[]) -> "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::(Camlrack.Symbol _)::[]))::[]) ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp ((Camlrack.SExp
      subgroup1)::[]))::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.SYMBOL) subgroup1 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp [])::_::[]) ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::[]))::_::[]) -> "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::(Camlrack.Symbol _)::[]))::_::[]) ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp ((Camlrack.SExp
      subgroup1)::[]))::_::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.SYMBOL) subgroup1 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp [])::_::_::[])
      -> "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::[]))::_::_::[]) -> "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::(Camlrack.Symbol _)::[]))::_::_::[]) ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp ((Camlrack.SExp
      subgroup1)::[]))::_::_::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.SYMBOL) subgroup1 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      [])::(Camlrack.SExp subgroup2)::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.ANY) subgroup2 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::[]))::(Camlrack.SExp subgroup2)::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.ANY) subgroup2 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp
      ((Camlrack.Symbol _)::(Camlrack.Symbol _)::[]))::(Camlrack.SExp
      subgroup2)::[]) when
      List.for_all (Camlrack.sexp_match Camlrack.ANY) subgroup2 ->
      "LAMBDA EXPRESSION"
  | Camlrack.SExp ((Camlrack.Symbol "lambda")::(Camlrack.SExp ((Camlrack.SExp
      subgroup1)::[]))::(Camlrack.SExp subgroup2)::[]) when
      (List.for_all (Camlrack.sexp_match Camlrack.ANY) subgroup2) &&
        (List.for_all (Camlrack.sexp_match Camlrack.SYMBOL) subgroup1)
      -> "LAMBDA EXPRESSION"
  | _ -> "OTHER"
