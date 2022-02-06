include Errors
include Sexp
include SexpPatterns

module Tokenize = Tokenize
module Parse = Parse
module Match = Match
module ListConvenienceFunctions = Match.ListConvenienceFunctions

let sexps_of_string (s : string) : (sexp list, Parse.parse_error) result = Parse.parse_many s

let sexps_of_string_exn (s : string) : sexp list = Parse.parse_many_exn s

let sexps_of_string_opt (s : string) : (sexp list) option = Parse.parse_many_opt s

let sexp_patterns_of_string (s : string) : (sexp_pattern list) option =
  match sexps_of_string_opt s with
  | Some ses ->
    let pat_opts = List.map sexp_pattern_of_sexp ses in
    if List.for_all Option.is_some pat_opts
    then Some (List.map Option.get pat_opts)
    else None
  | None -> None

let sexp_patterns_of_string_exn (s : string) : sexp_pattern list =
  let ses = sexps_of_string_exn s in
  let pat_opts = List.map sexp_pattern_of_sexp ses in
  if List.for_all Option.is_some pat_opts
  then List.map Option.get pat_opts
  else raise (CamlrackError "failed to convert one or more S-Expressions to S-Expression patterns")

let sexp_of_string (s : string) : (sexp, Parse.parse_error) result = Parse.parse s

let sexp_of_string_exn (s : string) : sexp = Parse.parse_exn s

let sexp_of_string_opt (s : string) : sexp option = Parse.parse_opt s

let sexp_pattern_of_string (s : string) : sexp_pattern option =
  match sexp_of_string_opt s with
  | Some se -> sexp_pattern_of_sexp se
  | None -> None

let sexp_pattern_of_string_exn (s : string) : sexp_pattern =
  match sexp_pattern_of_sexp (sexp_of_string_exn s) with
  | Some pat -> pat
  | None -> raise (CamlrackError "failed to convert S-Expression to S-Expression pattern")

let sexp_match (p : sexp_pattern) (se : sexp) : bool = Match.sexp_match p se
