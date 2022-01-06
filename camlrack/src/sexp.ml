type sexp =
  | Sexp of sexp list
  | Symbol of string
  | Integer of int

let rec string_of_sexp (s : sexp) : string =
  match s with
  | Sexp sexps -> "(" ^ String.concat " " (List.map string_of_sexp sexps) ^ ")"
  | Symbol s -> s
  | Integer i -> string_of_int i

let list_of_sexp (s : sexp) : sexp list =
  match s with
  | Sexp sexps -> sexps
  | _ -> failwith "not a list"
