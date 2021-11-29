open OUnit2

open Tokenize
open Tokenize.TokenTypes

let full_string_of_token (t : token) : string =
  match t with
  | LParen -> "LParen"
  | LBrace -> "LBrace"
  | LBracket -> "LBracket"
  | RParen -> "RParen"
  | RBrace -> "RBrace"
  | RBracket -> "RBracket"
  | Integer i ->
    if i < 0
    then "Integer (" ^ string_of_int i ^ ")"
    else "Integer " ^ string_of_int i
  | Symbol s -> "Symbol \"" ^ s ^ "\""

let full_string_of_tokens (ts : token list) : string =
  "[" ^ String.concat "; " (List.map full_string_of_token ts) ^ "]"

let test_tokenize (input, tokens : string * token list) _ =
  assert_equal ~printer:full_string_of_tokens tokens (tokenize input)

let simple_test_inputs =
  [ ("", [])
  ; ("()", [LParen; RParen])
  ; ("{}", [LBrace; RBrace])
  ; ("[]", [LBracket; RBracket])
  ; ("0", [Integer 0])
  ; ("-42", [Integer (-42)])
  ; ("foo", [Symbol "foo"])
  ]

let simple_tests =
  List.mapi
    (fun i p -> ("simple:" ^ string_of_int i ^ ":<\"" ^ fst p ^ "\">") >:: (test_tokenize p))
    simple_test_inputs

let test_inputs =
  [ ("(let (foo 42) (lambda (x) foo))",
    [LParen; Symbol "let"; LParen; Symbol "foo"; Integer 42; RParen;
     LParen; Symbol "lambda"; LParen; Symbol "x"; RParen; Symbol "foo";
     RParen; RParen])
  ]

let tests =
  List.mapi
    (fun i p -> ("test:" ^ string_of_int i ^ ":<\"" ^ fst p ^ "\">") >:: (test_tokenize p))
    test_inputs

let suite =
  "tokenize" >:::
  List.concat [simple_tests; tests]

let () = run_test_tt_main suite
