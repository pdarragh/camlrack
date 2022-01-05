open Camlrack
let test_match () =
  match sexp_of_string_exn "foo" with
  | Camlrack.Sexp ((Camlrack.Symbol lhs)::(Camlrack.Symbol rhs)::[]) ->
      "tuple: " ^ (lhs ^ rhs)
  | Camlrack.Sexp ((Camlrack.Symbol lhs)::(Camlrack.Sexp ((Camlrack.Symbol
      ilhs)::(Camlrack.Symbol irhs)::[]))::[]) ->
      "nested tuple: " ^ (lhs ^ (ilhs ^ irhs))
  | Camlrack.Symbol foo -> "variable: " ^ foo
  | _ -> "no match"
