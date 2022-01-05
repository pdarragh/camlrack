open Ppxlib

let process_pattern ~ctxt (p : pattern) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let open Ast_builder.Default in
  match p.ppat_desc with
  | Ppat_var varname -> [%pat? Camlrack.Symbol [%p ppat_var ~loc varname]]
  | _ -> p

let process_case ~ctxt case =
  { case with pc_lhs = process_pattern ~ctxt case.pc_lhs }

let expand ~ctxt e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let expand_cases cases = cases |> List.map (process_case ~ctxt) in
  let open Ast_helper.Exp in
  match e.pexp_desc with
  | Pexp_function cases -> function_ ~loc (expand_cases cases)
  | Pexp_match (e, cases) -> match_ ~loc e (expand_cases cases)
  | Pexp_try (e, cases) -> try_ ~loc e (expand_cases cases)
  | _ -> e

let ppx_camlrack =
  Extension.V3.declare
    "sexp"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand

let rule = Context_free.Rule.extension ppx_camlrack

let () = Driver.register_transformation ~rules:[ rule ] "ppx_camlrack"
