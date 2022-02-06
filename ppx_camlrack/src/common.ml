open Camlrack
open Ppxlib

(* The `errors`, `error`, and `warn` functions are based on implementations
   found in ppx_regexp:

   https://github.com/paurkedal/ppx_regexp/blob/master/ppx_regexp/ppx_regexp.ml#L28-L33
*)

let errors ~loc msg ~sub = raise (Location.Error (Location.Error.make ~loc msg ~sub))

let error ~loc msg = errors ~loc msg ~sub:[]

let warn ~loc msg e =
  let open Ast_helper in
  let e_msg = Exp.constant (Const.string msg) in
  let structure = {pstr_desc = Pstr_eval (e_msg, []); pstr_loc = loc} in
  Exp.attr e (Attr.mk ~loc {txt = "ocaml.ppwarning"; loc} (PStr [structure]))

let rec sexp_pat_of_pattern (p : pattern) : pattern option =
  let open Ast_builder.Default in
  let rec convert_pat (p : pattern) : pattern option =
    let loc = p.ppat_loc in
    match p.ppat_desc with
    (* Wildcards remain wildcards. *)
    | Ppat_any -> Some (ppat_any ~loc)
    (* Constants are trivially converted to matching constants. *)
    | Ppat_constant (Pconst_char c) ->
      Some [%pat? Camlrack.String [%p pstring ~loc (String.make 1 c)]]
    | Ppat_constant (Pconst_string (s, loc, _)) ->
      Some [%pat? Camlrack.String [%p pstring ~loc s]]
    | Ppat_constant (Pconst_integer (s, None)) ->
      Some [%pat? Camlrack.Integer [%p ppat_constant ~loc (Pconst_integer (s, None))]]
    | Ppat_constant (Pconst_float (s, None)) ->
      Some [%pat? Camlrack.Float [%p pfloat ~loc s]]
    (* Variables become symbols with the same name. *)
    | Ppat_var {txt = name; loc = loc} ->
      Some [%pat? Camlrack.Symbol [%p ppat_var ~loc {txt = name; loc = loc}]]
    (* Tuples are converted recursively. *)
    | Ppat_tuple pats ->
      let converted_pats = List.map convert_pat pats in
      if List.for_all Option.is_some converted_pats
      then
        let pats' = List.map Option.get converted_pats in
        Some [%pat? Camlrack.SExp [%p plist ~loc pats']]
      else
        None
    (* Anything else is not converted. *)
    | _ -> None in
  let rec convert_sexp ~loc (s : sexp) : pattern =
    match s with
    | Integer i -> [%pat? Camlrack.Integer [%p pint ~loc i]]
    | Float f -> [%pat? Camlrack.Float [%p pfloat ~loc (string_of_float f)]]
    | String s -> [%pat? Camlrack.String [%p pstring ~loc s]]
    | Symbol s -> [%pat? Camlrack.Symbol [%p ppat_var ~loc {txt = s; loc = loc}]]
    | SExp ses -> [%pat? Camlrack.SExp [%p plist ~loc (List.map (convert_sexp ~loc) ses)]]
  in
  match p.ppat_desc with
  (* If the entire pattern is one string literal, we parse that string and
     attempt to convert that S-Expression to a pattern. *)
  | Ppat_constant (Pconst_string (s, loc, _)) ->
    (match Camlrack.sexp_of_string_opt s with
     | Some se -> Some (convert_sexp ~loc se)
     | None -> None)
  (* An or-pattern is just handled recursively. *)
  | Ppat_or (p1, p2) ->
    (match sexp_pat_of_pattern p1, sexp_pat_of_pattern p2 with
     | Some p1', Some p2' -> Some (ppat_or ~loc:p.ppat_loc p1' p2')
     | _ -> None)
  (* If not a string literal, we will convert variables/constants/tuples to
     S-Expressions manually. *)
  | _ -> convert_pat p

let sexp_exp_of_expression (e : expression) : expression option =
  let open Ast_builder.Default in
  let rec convert_exp (e : expression) : expression option =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    (* Constants are trivially converted to matching constants. *)
    | Pexp_constant (Pconst_char c) ->
      Some [%expr Camlrack.String [%e estring ~loc (String.make 1 c)]]
    | Pexp_constant (Pconst_string (s, loc, _)) ->
      Some [%expr Camlrack.String [%e estring ~loc s]]
    | Pexp_constant (Pconst_integer (s, None)) ->
      Some [%expr Camlrack.Integer [%e pexp_constant ~loc (Pconst_integer (s, None))]]
    | Pexp_constant (Pconst_float (s, None)) ->
      Some [%expr Camlrack.Float [%e efloat ~loc s]]
    (* Variables are assumed to be literal symbols. *)
    | Pexp_ident {txt = (Lident name); loc = loc} ->
      Some [%expr Camlrack.Symbol [%e estring ~loc name]]
    (* Tuples are converted recursively. *)
    | Pexp_tuple exps ->
      let converted_exps = List.map convert_exp exps in
      if List.for_all Option.is_some converted_exps
      then
        let exps' = List.map Option.get converted_exps in
        Some [%expr Camlrack.SExp [%e elist ~loc exps']]
      else
        None
    (* Anything else is not converted. *)
    | _ -> None in
  let rec convert_sexp ~loc (s : sexp) : expression =
    match s with
    | Integer i -> [%expr Camlrack.Integer [%e eint ~loc i]]
    | Float f -> [%expr Camlrack.Float [%e efloat ~loc (string_of_float f)]]
    | String s -> [%expr Camlrack.String [%e estring ~loc s]]
    | Symbol s -> [%expr Camlrack.Symbol [%e estring ~loc s]]
    | SExp ses -> [%expr Camlrack.SExp [%e elist ~loc (List.map (convert_sexp ~loc) ses)]]
  in
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (s, loc, _)) ->
    (match Camlrack.sexp_of_string_opt s with
     | Some se -> Some (convert_sexp ~loc se)
     | None -> None)
  | _ ->
    (match convert_exp e with
     | Some ne -> Some ne
     | None -> None)
