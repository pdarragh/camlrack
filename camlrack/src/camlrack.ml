include Sexp

module Tokenize = Tokenize
module Parse = Parse
module Match = Match

let sexp_of_string (s : string) : sexp option = Parse.parse s

let sexp_of_string_exn (s : string) : sexp = Parse.parse_exn s
