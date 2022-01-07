type sexp =
  | Integer of int
  | Float of float
  | String of string
  | Symbol of string
  | SExp of sexp list

let rec full_string_of_sexp (se : sexp) : string =
  match se with
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
  | SExp sexps -> "(" ^ String.concat " " (List.map full_string_of_sexp sexps) ^ ")"

module Errors = struct
  let sexp_not_a (s : string) (se : sexp) : 'a =
    failwith ("S-Expression is not a " ^ s ^ ": " ^ full_string_of_sexp se)
  let sexp_not_an (s : string) (se : sexp) : 'a =
    failwith ("S-Expression is not an " ^ s ^ ": " ^ full_string_of_sexp se)
end

(* The following functions are based heavily (almost exactly copied) from the
   PLAIT language by Matthew Flatt:

   https://docs.racket-lang.org/plait/Predefined_Functions_and_Constants.html#%28part._.S-.Expressions%29
*)

let list_of_sexp_opt (se : sexp) : (sexp list) option =
  match se with
  | SExp sexps -> Some sexps
  | _ -> None

let list_of_sexp (se : sexp) : sexp list =
  match list_of_sexp_opt se with
  | Some ses -> ses
  | None -> Errors.sexp_not_a "list" se

let sexp_of_list (ses : sexp list) : sexp = SExp ses

let int_of_sexp_opt (se : sexp) : int option =
  match se with
  | Integer i -> Some i
  | _ -> None

let int_of_sexp (se : sexp) : int =
  match int_of_sexp_opt se with
  | Some i -> i
  | None -> Errors.sexp_not_an "integer" se

let sexp_of_int (i : int) : sexp = Integer i

let float_of_sexp_opt (se : sexp) : float option =
  match se with
  | Float f -> Some f
  | _ -> None

let float_of_sexp (se : sexp) : float =
  match float_of_sexp_opt se with
  | Some f -> f
  | None -> Errors.sexp_not_a "float" se

let sexp_of_float (f : float) : sexp = Float f

let string_of_sexp_opt (se : sexp) : string option =
  match se with
  | String s -> Some s
  | _ -> None

let string_of_sexp (se : sexp) : string =
  match string_of_sexp_opt se with
  | Some s -> s
  | None -> Errors.sexp_not_a "string" se

let sexp_of_string (s : string) : sexp = String s

let symbol_of_sexp_opt (se : sexp) : string option =
  match se with
  | Symbol s -> Some s
  | _ -> None

let symbol_of_sexp (se : sexp) : string =
  match symbol_of_sexp_opt se with
  | Some s -> s
  | None -> Errors.sexp_not_a "symbol" se

let sexp_of_symbol (s : string) : sexp = Symbol s
