type sexp =
  | SExp of sexp list
  | Symbol of string
  | Integer of int
  | Float of float
  | String of string

let rec string_of_sexp (s : sexp) : string =
  match s with
  | SExp sexps -> "(" ^ String.concat " " (List.map string_of_sexp sexps) ^ ")"
  | Symbol s -> s
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""

let list_of_sexp (s : sexp) : sexp list =
  match s with
  | SExp sexps -> sexps
  | _ -> failwith "not a list"
