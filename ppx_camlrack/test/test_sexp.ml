open Camlrack
let test_match () =
  match%sexp sexp_of_string_exn "foo" with
  | (lhs, rhs) -> "tuple: " ^ (lhs ^ rhs)
  | (lhs, (ilhs, irhs)) -> "nested tuple: " ^ (lhs ^ (ilhs ^ irhs))
  | "((ilhs irhs) rhs)" -> "string sexp: " ^ (ilhs ^ (irhs ^ rhs))
  | "(1 foo)" -> "string sexp with integer: " ^ foo
  | foo -> "variable: " ^ foo
  | _ -> "no match"

let test_at = [%sexp "(foo 1 (+ 2.0 x))"]
