open Errors

type sexp =
  | Integer of int
  | Float of float
  | String of string
  | Symbol of string
  | SExp of sexp list

let rec render_string_of_sexp (se : sexp) : string =
  match se with
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
  | SExp sexps -> "(" ^ String.concat " " (List.map render_string_of_sexp sexps) ^ ")"

let rec repr_string_of_sexp (se : sexp) : string =
  match se with
  | Integer i -> "Integer " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | String s -> {|String "|} ^ s ^ {|"|}
  | Symbol s -> {|Symbol "|} ^ s ^ {|"|}
  | SExp sexps -> "SExp [" ^ String.concat "; " (List.map repr_string_of_sexp sexps) ^ "]"

let sexp_not_err (desired_type : string) (se : sexp) : 'a =
  let article =
    if List.exists ((=) desired_type.[0]) ['a'; 'e'; 'i'; 'o'; 'u']
    then "an"
    else "a" in
  raise (CamlrackError ("S-Expression is not " ^ article ^ " " ^ desired_type ^ ": " ^ render_string_of_sexp se))

(* The following functions are based heavily (almost exactly copied) from the
   PLAIT language by Matthew Flatt:

   https://docs.racket-lang.org/plait/Predefined_Functions_and_Constants.html#%28part._.S-.Expressions%29
*)

let sexp_to_list_opt (se : sexp) : (sexp list) option =
  match se with
  | SExp sexps -> Some sexps
  | _ -> None

let sexp_to_list (se : sexp) : sexp list =
  match sexp_to_list_opt se with
  | Some ses -> ses
  | None -> sexp_not_err "list" se

let list_to_sexp (ses : sexp list) : sexp = SExp ses

let sexp_to_int_opt (se : sexp) : int option =
  match se with
  | Integer i -> Some i
  | _ -> None

let sexp_to_int (se : sexp) : int =
  match sexp_to_int_opt se with
  | Some i -> i
  | None -> sexp_not_err "integer" se

let int_to_sexp (i : int) : sexp = Integer i

let sexp_to_float_opt (se : sexp) : float option =
  match se with
  | Float f -> Some f
  | _ -> None

let sexp_to_float (se : sexp) : float =
  match sexp_to_float_opt se with
  | Some f -> f
  | None -> sexp_not_err "float" se

let float_to_sexp (f : float) : sexp = Float f

let sexp_to_string_opt (se : sexp) : string option =
  match se with
  | String s -> Some s
  | _ -> None

let sexp_to_string (se : sexp) : string =
  match sexp_to_string_opt se with
  | Some s -> s
  | None -> sexp_not_err "string" se

let string_to_sexp (s : string) : sexp = String s

let sexp_to_symbol_opt (se : sexp) : string option =
  match se with
  | Symbol s -> Some s
  | _ -> None

let sexp_to_symbol (se : sexp) : string =
  match sexp_to_symbol_opt se with
  | Some s -> s
  | None -> sexp_not_err "symbol" se

let symbol_to_sexp (s : string) : sexp = Symbol s
