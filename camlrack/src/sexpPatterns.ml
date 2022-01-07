open Sexp

type sexp_pattern =
  | SYMBOL
  | NUMBER
  | INTEGER
  | FLOAT
  | STRING
  | ANY
  | PInteger of int
  | PFloat of float
  | PString of string
  | PSymbol of string
  | SPat of sexp_pattern list

let rec string_of_sexp_pattern (pat : sexp_pattern) : string =
  match pat with
  | SYMBOL -> "SYMBOL"
  | NUMBER -> "NUMBER"
  | INTEGER -> "INTEGER"
  | FLOAT -> "FLOAT"
  | STRING -> "STRING"
  | ANY -> "ANY"
  | PInteger i -> string_of_int i
  | PFloat f -> string_of_float f
  | PString s -> "\"" ^ s ^ "\""
  | PSymbol s -> s
  | SPat pats -> "{" ^ String.concat " " (List.map string_of_sexp_pattern pats) ^ "}"

let rec sexp_pattern_of_sexp (sexp : sexp) : sexp_pattern =
  match sexp with
  | Symbol "SYMBOL" -> SYMBOL
  | Symbol "NUMBER" -> NUMBER
  | Symbol "INTEGER" -> INTEGER
  | Symbol "FLOAT" -> FLOAT
  | Symbol "STRING" -> STRING
  | Symbol "ANY" -> ANY
  | Symbol s -> PSymbol s
  | Integer i -> PInteger i
  | Float f -> PFloat f
  | String s -> PString s
  | SExp sexps -> SPat (List.map sexp_pattern_of_sexp sexps)

let list_of_sexp_pattern (pat : sexp_pattern) : sexp_pattern list =
  match pat with
  | SPat pats -> pats
  | _ -> failwith "pattern not a list"
