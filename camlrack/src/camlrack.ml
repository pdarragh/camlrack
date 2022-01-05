module Tokenize = Tokenize
module Parse = Parse

include Parse.Sexp

let sexp_of_string (s : string) : sexp option = Parse.parse s

let sexp_of_string_exn (s : string) : sexp = Parse.parse_exn s
