open Sexp

type sexp_pattern =
  | SYMBOL
  | INTEGER
  | FLOAT
  | STRING
  | ANY
  | PInteger of int
  | PFloat of float
  | PString of string
  | PSymbol of string
  | SPat of sexp_pattern list

let rec render_string_of_sexp_pattern (pat : sexp_pattern) : string =
  match pat with
  | SYMBOL -> "SYMBOL"
  | INTEGER -> "INTEGER"
  | FLOAT -> "FLOAT"
  | STRING -> "STRING"
  | ANY -> "ANY"
  | PInteger i -> string_of_int i
  | PFloat f -> string_of_float f
  | PString s -> "\"" ^ s ^ "\""
  | PSymbol s -> s
  | SPat pats -> "{" ^ String.concat " " (List.map render_string_of_sexp_pattern pats) ^ "}"

let rec repr_string_of_sexp_pattern (pat : sexp_pattern) : string =
  match pat with
  | SYMBOL -> "SYMBOL"
  | INTEGER -> "INTEGER"
  | FLOAT -> "FLOAT"
  | STRING -> "STRING"
  | ANY -> "ANY"
  | PInteger i -> "PInteger " ^ string_of_int i
  | PFloat f -> "PFloat " ^ string_of_float f
  | PString s -> {|PString "|} ^ s ^ {|"|}
  | PSymbol s -> {|PSymbol "|} ^ s ^ {|"|}
  | SPat pats -> "SPat [" ^ String.concat "; " (List.map repr_string_of_sexp_pattern pats) ^ "]"

let sexp_pattern_of_sexp (sexp : sexp) : sexp_pattern option =
  let rec valid_pattern (sexp : sexp) : bool =
    match sexp with
    | SExp ses -> valid_patterns ses false
    | Integer _ | Float _ | String _ -> true
    | Symbol "..." -> false
    | Symbol _ -> true
  and valid_patterns (sexps : sexp list) (saw_dots : bool) : bool =
    match sexps with
    | [] -> true
    | _ :: Symbol "..." :: ses ->
      if saw_dots
      then false
      else valid_patterns ses true
    | se :: ses -> valid_pattern se && valid_patterns ses saw_dots
  in
  let rec build_pattern (sexp : sexp) : sexp_pattern =
    match sexp with
    | Symbol "SYMBOL" -> SYMBOL
    | Symbol "INTEGER" -> INTEGER
    | Symbol "FLOAT" -> FLOAT
    | Symbol "STRING" -> STRING
    | Symbol "ANY" -> ANY
    | Symbol s -> PSymbol s
    | Integer i -> PInteger i
    | Float f -> PFloat f
    | String s -> PString s
    | SExp sexps -> SPat (List.map build_pattern sexps)
  in
  if valid_pattern sexp
  then Some (build_pattern sexp)
  else None
