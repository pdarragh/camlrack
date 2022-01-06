open OUnit2

open Camlrack.Tokenize

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
  | Float f ->
    if f < 0.0
    then "Float (" ^ string_of_float f ^ ")"
    else "Float " ^ string_of_float f
  | String s -> "String \"" ^ s ^ "\""
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
  ; ("+42", [Integer 42])
  ; ("13.", [Float 13.])
  ; ("6.9", [Float 6.9])
  ; ("+6.9", [Float 6.9])
  ; ("-13.", [Float (-13.)])
  ; ("1e1", [Float 1e1])
  ; ("10e-1", [Float 10e-1])
  ; ("10e+1", [Float 10e+1])
  ; ({|some "string"|}, [Symbol "some"; String "string"])
  ; ("foo", [Symbol "foo"])
  ; ("+", [Symbol "+"])
  ; (">>=", [Symbol ">>="])
  ; ("123 +~ 10.2e3 - (+ x 2)", [ Integer 123; Symbol "+~"; Float 10.2e3;
                                  Symbol "-"; LParen; Symbol "+";
                                  Symbol "x"; Integer 2; RParen ])
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
