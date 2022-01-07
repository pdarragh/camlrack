open Camlrack
let test_match () =
  match sexp_of_string_exn "foo" with
  | Camlrack.SExp ((Camlrack.Symbol lhs)::(Camlrack.Symbol rhs)::[]) ->
      "tuple: " ^ (lhs ^ rhs)
  | Camlrack.SExp ((Camlrack.Symbol lhs)::(Camlrack.SExp ((Camlrack.Symbol
      ilhs)::(Camlrack.Symbol irhs)::[]))::[]) ->
      "nested tuple: " ^ (lhs ^ (ilhs ^ irhs))
  | Camlrack.SExp ((Camlrack.SExp ((Camlrack.Symbol ilhs)::(Camlrack.Symbol
      irhs)::[]))::(Camlrack.Symbol rhs)::[]) ->
      "string sexp: " ^ (ilhs ^ (irhs ^ rhs))
  | Camlrack.SExp ((Camlrack.Integer 1)::(Camlrack.Symbol foo)::[]) ->
      "string sexp with integer: " ^ foo
  | Camlrack.Symbol foo -> "variable: " ^ foo
  | _ -> "no match"
let test_at =
  Camlrack.SExp
    [Camlrack.Symbol "foo";
    Camlrack.Integer 1;
    Camlrack.SExp
      [Camlrack.Symbol "+"; Camlrack.Float 2.; Camlrack.Symbol "x"]]
