type value =
  | Symbol of string
  | Integer of int

type sexp =
  | Sexp of sexp list
  | Value of value
