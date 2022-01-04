type token =
  | LParen
  | LBrace
  | LBracket
  | RParen
  | RBrace
  | RBracket
  | Integer of int
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
  | Symbol s -> s
