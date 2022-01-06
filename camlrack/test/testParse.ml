open OUnit2

open Camlrack.Parse

let test_parse (input, sexp : string * sexp) _ =
  assert_equal sexp (parse_exn input)

let parse_tests =
  [ ("123", Integer 123)
  ; ("-10", Integer (-10))
  ; ("123.", Float 123.)
  ; ("-123.", Float (-123.))
  ; ("1e1", Float 10.)
  ; ("13.e-1", Float 1.3)
  ; ("foo", Symbol "foo")
  ; ("(foo (bar baz) quux)",
     SExp
       [ Symbol "foo"
       ; SExp
           [ Symbol "bar"
           ; Symbol "baz" ]
       ; Symbol "quux" ])
  ; ("{lambda {SYMBOL ...} ANY ...}",
     SExp
       [ Symbol "lambda"
       ; SExp
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
