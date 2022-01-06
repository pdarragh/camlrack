open Camlrack
open Ppxlib

let rec pattern_to_sexp_pat (p : pattern) : pattern option =
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
    (match Camlrack.sexp_of_string s with
     | Some se -> Some (convert_sexp ~loc se)
     | None -> None)
  (* If not a string literal, we will convert variables/constants/tuples to
     S-Expressions manually. *)
  | Ppat_var {txt = name; loc = loc} ->
    Some [%pat? Camlrack.Symbol [%p ppat_var ~loc {txt = name; loc = loc}]]
  (* An or-pattern is just handled recursively. *)
  | Ppat_or (p1, p2) ->
    (match pattern_to_sexp_pat p1, pattern_to_sexp_pat p2 with
     | Some p1', Some p2' -> Some (ppat_or ~loc:p.ppat_loc p1' p2')
     | _ -> None)
  (* In any other case, we do not convert to an S-Expression. *)
  | _ -> convert_pat p

let rec process_pattern (p : pattern) =
  match pattern_to_sexp_pat p with
  | Some p' -> p'
  | None -> p

let process_case case =
  { case with pc_lhs = process_pattern case.pc_lhs }

let expand_sexp ~ctxt e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expand_cases cases = List.map process_case cases in
  let open Ast_helper.Exp in
  match e.pexp_desc with
  | Pexp_function cases -> function_ ~loc (expand_cases cases)
  | Pexp_match (e, cases) -> match_ ~loc e (expand_cases cases)
  | Pexp_try (e, cases) -> try_ ~loc e (expand_cases cases)
  | _ -> e

let ppx_sexp =
  Extension.V3.declare
    "sexp"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_sexp

let sexp_rule = Context_free.Rule.extension ppx_sexp
