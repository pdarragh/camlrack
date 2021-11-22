type token =
  | LParen
  | RParen
  | Integer of int
  | Symbol of string

let string_of_token (t : token) : string =
  match t with
  | LParen -> "("
  | RParen -> ")"
  | Integer i -> string_of_int i
  | Symbol s -> s
