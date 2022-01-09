include Sexp
include SexpPatterns

module Tokenize = Tokenize
module Parse = Parse
module Match = Match
module ListConvenienceFunctions = Match.ListConvenienceFunctions

let sexp_of_string (s : string) : sexp option = Parse.parse s

let sexp_of_string_exn (s : string) : sexp = Parse.parse_exn s

let sexp_pattern_of_string (s : string) : sexp_pattern option =
  match sexp_of_string s with
  | Some se -> sexp_pattern_of_sexp se
  | None -> None

let sexp_pattern_of_string_exn (s : string) : sexp_pattern =
  match sexp_pattern_of_sexp (sexp_of_string_exn s) with
  | Some pat -> pat
  | None -> failwith "invalid pattern"

let sexp_match (p : sexp_pattern) (se : sexp) : bool = Match.sexp_match p se
