open OUnit2

open Camlrack.Parse
open Camlrack.Match

let test_match (raw_pat, raw_sexp : string * string) _ =
  let pat = sexp_pattern_of_sexp (parse_exn raw_pat) in
  let sexp = parse_exn raw_sexp in
  assert_equal true (sexp_match pat sexp)

let match_tests =
  [ ("1", "1")
  ; ("NUMBER", "1")
  ; ("NUMBER", "-1")
  ; ("lambda", "lambda")
  ; ("SYMBOL", "lambda")
  ; ("SYMBOL", ">>=")
  ; ("ANY", "1")
  ; ("ANY", "foo")
  ; ("ANY", "(foo bar)")
  ; ("{NUMBER SYMBOL}", "(1 foo)")
  ; ("{NUMBER {SYMBOL SYMBOL}}", "(1 (lhs rhs))")
  ; ("{lambda {SYMBOL} ANY}", "(lambda (x) (1 2 3))")
  ]

let ellipsis_tests =
  [ ("{NUMBER ...}",
     [ "(1)"
     ; "(1 2)"
     ; "(1 2 3)"
     ])
  ; ("{SYMBOL ...}",
     [ "(foo)"
     ; "(foo bar)"
     ])
  ; ("{ANY ...}",
     [ "(foo)"
     ; "(foo 1)"
     ; "(foo (1 2 3) bar)"
     ])
  ; ("{lambda NUMBER ... SYMBOL}",
     [ "(lambda 1 foo)"
     ; "(lambda 1 2 foo)"
     ; "(lambda 1 2 3 foo)"
     ])
  ; ("{lambda {SYMBOL ...} ANY ...}",
     [ "(lambda () 1)"
     ; "(lambda () (1 2) (three four))"
     ; "(lambda (x) body)"
     ; "(lambda (x y z) (+ x (* y z*)))"
     ])
  ]

let flatten_pairwise (ps : ('a * ('b list)) list) : ('a * 'b) list =
  List.concat (List.map (fun (l, rs) -> List.map (fun r -> (l, r)) rs) ps)

let tests =
  List.append
    (List.mapi
       (fun i p -> ("test:" ^ string_of_int i ^ ":<\"" ^ fst p ^ "\">") >:: (test_match p))
       match_tests)
    (List.mapi
       (fun i p -> ("test:" ^ string_of_int (i + List.length match_tests) ^ ":<\"" ^ fst p ^ "\">")
                   >:: (test_match p))
       (flatten_pairwise ellipsis_tests))

let suite =
  "parse" >:::
  List.concat [tests]

let () = run_test_tt_main suite
