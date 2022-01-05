open Camlrack
let test_match () =
  match sexp_of_string_exn "foo" with
  | Camlrack.Symbol foo -> "did it"
  | _ -> "did not do it"
