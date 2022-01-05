open Camlrack
let test_match () =
  match%sexp sexp_of_string_exn "foo" with
  | (lhs, rhs) -> "tuple: " ^ (lhs ^ rhs)
  | (lhs, (ilhs, irhs)) -> "nested tuple: " ^ (lhs ^ (ilhs ^ irhs))
  | foo -> "variable: " ^ foo
  | _ -> "no match"
