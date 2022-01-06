open Sexp

type sexp_pattern =
  | SYMBOL
  | NUMBER
  | ANY
  | Symbol of string
  | Number of int
  | SPat of sexp_pattern list

let rec string_of_sexp_pattern (pat : sexp_pattern) : string =
  match pat with
  | SYMBOL -> "SYMBOL"
  | NUMBER -> "NUMBER"
  | ANY -> "ANY"
  | Symbol s -> s
  | Number i -> string_of_int i
  | SPat pats -> "{" ^ String.concat " " (List.map string_of_sexp_pattern pats) ^ "}"

let rec sexp_pattern_of_sexp (sexp : sexp) : sexp_pattern =
  match sexp with
  | Symbol "SYMBOL" -> SYMBOL
  | Symbol "NUMBER" -> NUMBER
  | Symbol "ANY" -> ANY
  | Symbol s -> Symbol s
  | Integer i -> Number i
  | Sexp sexps -> SPat (List.map sexp_pattern_of_sexp sexps)

let list_of_sexp_pattern (pat : sexp_pattern) : sexp_pattern list =
  match pat with
  | SPat pats -> pats
  | _ -> failwith "pattern not a list"
