type token =
  | LParen
  | LBrace
  | LBracket
  | RParen
  | RBrace
  | RBracket
  | Integer of int
  | Float of float
  | String of string
  | Symbol of string

let string_of_token (t : token) : string =
  match t with
  | LParen -> "("
  | LBrace -> "{"
  | LBracket -> "["
  | RParen -> ")"
  | RBrace -> "}"
  | RBracket -> "]"
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
