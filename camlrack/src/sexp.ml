type sexp =
  | Integer of int
  | Float of float
  | String of string
  | Symbol of string
  | SExp of sexp list

let rec string_of_sexp (s : sexp) : string =
  match s with
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
  | SExp sexps -> "(" ^ String.concat " " (List.map string_of_sexp sexps) ^ ")"

let list_of_sexp (s : sexp) : sexp list =
  match s with
  | SExp sexps -> sexps
  | _ -> failwith ("S-Expression is not a list: " ^ string_of_sexp s)
