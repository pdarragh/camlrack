open Common
open Ppxlib

let process_pattern (p : pattern) =
  match sexp_pat_of_pattern p with
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
  | _ ->
    (match sexp_exp_of_expression e with
     | Some ne -> ne
     | None -> e)

let ppx_sexp =
  Extension.V3.declare
    "sexp"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_sexp

let sexp_rule = Context_free.Rule.extension ppx_sexp
