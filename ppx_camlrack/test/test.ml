open Camlrack

let test_match () =
  match%sexp sexp_of_string_exn "foo" with
  | foo -> "did it"
  | _ -> "did not do it"
