open Camlrack.Match
open Ppxlib

let process_scrutinee (e : expression) : expression =
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
    (match Camlrack.sexp_of_string s with
     | Some se -> convert_sexp ~loc se
     | None -> e)
  | _ ->
    (match convert_exp e with
     | Some ne -> ne
     | None -> e)

let pattern_to_sexp_pattern_exp (p : pattern) : expression option =
  let open Ast_builder.Default in
  let rec convert_sexp_pattern ~loc (pat : sexp_pattern) : expression =
    match pat with
    | SYMBOL -> [%expr Camlrack.Match.SYMBOL]
    | NUMBER -> [%expr Camlrack.Match.NUMBER]
    | INTEGER -> [%expr Camlrack.Match.INTEGER]
    | FLOAT -> [%expr Camlrack.Match.FLOAT]
    | STRING -> [%expr Camlrack.Match.STRING]
    | ANY -> [%expr Camlrack.Match.ANY]
    | Integer i -> [%expr Camlrack.Match.Integer [%e eint ~loc i]]
    | Float f -> [%expr Camlrack.Match.Float [%e efloat ~loc (string_of_float f)]]
    | String s -> [%expr Camlrack.Match.String [%e estring ~loc s]]
    | Symbol s -> [%expr Camlrack.Match.Symbol [%e estring ~loc s]]
    | SPat pats -> [%expr Camlrack.Match.SPat [%e elist ~loc (List.map (convert_sexp_pattern ~loc) pats)]]
  in
  match p.ppat_desc with
  | Ppat_constant (Pconst_string (s, loc, _)) ->
    (match Camlrack.sexp_of_string s with
     | Some se -> Some (convert_sexp_pattern ~loc (sexp_pattern_of_sexp se))
     | None -> None)
  | _ -> None

let build_if_chain ~ctxt (s : expression) (cs : case list) : expression option =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let open Ast_builder.Default in
  let input_sexp = process_scrutinee s in
  let process_nonfinal_case (c : case) (else_expr: expression) : expression option =
    let loc = c.pc_lhs.ppat_loc in
    (match c.pc_lhs.ppat_desc with
     | Ppat_any ->
       (match c.pc_guard with
        | None -> Some (pexp_ifthenelse ~loc [%expr true] c.pc_rhs (Some else_expr))
        | Some exp -> Some (pexp_ifthenelse ~loc exp c.pc_rhs (Some else_expr)))
     | _ ->
       (match pattern_to_sexp_pattern_exp c.pc_lhs with
        | None -> None
        | Some sexp_pat ->
          let cond = [%expr Camlrack.Match.sexp_match [%e sexp_pat] sexp] in
          let cond =
            (match c.pc_guard with
             | None -> cond
             | Some exp -> [%expr [%e cond] && [%e exp]]) in
          Some (pexp_ifthenelse ~loc cond c.pc_rhs (Some else_expr)))) in
  let process_final_case (c : case) : expression option =
    let loc = c.pc_lhs.ppat_loc in
    (match c.pc_lhs.ppat_desc with
     | Ppat_any ->
       (match c.pc_guard with
        | None -> Some c.pc_rhs
        | Some exp -> Some (pexp_ifthenelse ~loc exp c.pc_rhs (Some [%expr failwith "Unhandled S-Expression wildcard."])))
     | _ -> process_nonfinal_case c [%expr failwith "Unhandled S-Expression."]) in
  let rec build_if_chain (cs : case list) : expression option =
    match cs with
    (* Cannot construct a `match` without at least one case, so this is just
       for the pattern exhaustiveness. *)
    | [] -> None
    (* If the last case is a wildcard, its body becomes the body of the
       `else`. Otherwise, the last case becomes the last `if` and a failing
       `else` is generated to handle the unknown case. *)
    | c::[] -> process_final_case c
    (* There are multiple cases left, so generate the if-expression for the
       remainder and use it as the body of the `else` for the current case. *)
    | c::cs' ->
      (match build_if_chain cs' with
       | None -> None
       | Some else_expr -> process_nonfinal_case c else_expr)
  in
  match build_if_chain cs with
  | None -> None
  | Some chain ->
    Some (pexp_let
            ~loc
            Nonrecursive
            [{pvb_pat = ppat_var ~loc {txt = "sexp"; loc = loc}
             ; pvb_expr = input_sexp
             ; pvb_attributes = []
             ; pvb_loc = loc}]
            chain)

let expand_spat ~ctxt e =
  let _ = Expansion_context.Extension.extension_point_loc ctxt in
  match e.pexp_desc with
  | Pexp_match (scrut, cases) ->
    (* Only replace the match-expression if we are successful. *)
    (match build_if_chain ~ctxt scrut cases with
     | Some expr -> expr
     | None -> e)
  | _ -> e

let ppx_spat =
  Extension.V3.declare
    "spat"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_spat

let spat_rule = Context_free.Rule.extension ppx_spat
