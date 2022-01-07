include Sexp
include SexpPatterns

module Tokenize = Tokenize
module Parse = Parse
module Match = Match

let sexp_of_string (s : string) : sexp option = Parse.parse s

let sexp_of_string_exn (s : string) : sexp = Parse.parse_exn s

let sexp_match (p : sexp_pattern) (se : sexp) : bool = Match.sexp_match p se
