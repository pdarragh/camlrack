open OUnit2

open Camlrack.Parse
open Camlrack.Parse.Sexp

let test_parse (input, sexp : string * sexp) _ =
  assert_equal sexp (parse_exn input)

let parse_tests =
  [ ("123", Integer 123)
  ; ("foo", Symbol "foo")
  ; ("(foo (bar baz) quux)",
     Sexp
       [ Symbol "foo"
       ; Sexp
           [ Symbol "bar"
           ; Symbol "baz" ]
       ; Symbol "quux" ])
  ; ("{lambda {SYMBOL ...} ANY ...}",
     Sexp
       [ Symbol "lambda"
       ; Sexp
           [ Symbol "SYMBOL"
           ; Symbol "..." ]
       ; Symbol "ANY"
       ; Symbol "..." ])
  ]

let tests =
  List.mapi
    (fun i p -> ("test:" ^ string_of_int i ^ ":<\"" ^ fst p ^ "\">") >:: (test_parse p))
    parse_tests

let suite =
  "parse" >:::
  List.concat [tests]

let () = run_test_tt_main suite
