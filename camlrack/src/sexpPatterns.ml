open Sexp

type sexp_pattern =
  | SYMBOL
  | NUMBER
  | INTEGER
  | FLOAT
  | STRING
  | ANY
  | Symbol of string
  | Integer of int
  | Float of float
  | String of string
  | SPat of sexp_pattern list

let rec string_of_sexp_pattern (pat : sexp_pattern) : string =
  match pat with
  | SYMBOL -> "SYMBOL"
  | NUMBER -> "NUMBER"
  | INTEGER -> "INTEGER"
  | FLOAT -> "FLOAT"
  | STRING -> "STRING"
  | ANY -> "ANY"
  | Symbol s -> s
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | SPat pats -> "{" ^ String.concat " " (List.map string_of_sexp_pattern pats) ^ "}"

let rec sexp_pattern_of_sexp (sexp : sexp) : sexp_pattern =
  match sexp with
  | Symbol "SYMBOL" -> SYMBOL
  | Symbol "NUMBER" -> NUMBER
  | Symbol "INTEGER" -> INTEGER
  | Symbol "FLOAT" -> FLOAT
  | Symbol "STRING" -> STRING
  | Symbol "ANY" -> ANY
  | Symbol s -> Symbol s
  | Integer i -> Integer i
  | Float f -> Float f
  | String s -> String s
  | SExp sexps -> SPat (List.map sexp_pattern_of_sexp sexps)

let list_of_sexp_pattern (pat : sexp_pattern) : sexp_pattern list =
  match pat with
  | SPat pats -> pats
  | _ -> failwith "pattern not a list"
