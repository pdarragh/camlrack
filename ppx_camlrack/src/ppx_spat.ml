open Common
open Camlrack.Match
open Ppxlib

let ( <&> ) (x : 'a option) (f : 'a -> 'b) : 'b option = Option.map f x

let scrutinee_match_name = "sexp"

let rec sexp_pattern_exp_of_sexp_pattern ~loc (spat : sexp_pattern) : expression =
  let open Ast_builder.Default in
  match spat with
  | SYMBOL     -> [%expr Camlrack.SYMBOL]
  | INTEGER    -> [%expr Camlrack.INTEGER]
  | FLOAT      -> [%expr Camlrack.FLOAT]
  | STRING     -> [%expr Camlrack.STRING]
  | ANY        -> [%expr Camlrack.ANY]
  | PInteger i -> [%expr Camlrack.PInteger [%e eint ~loc i]]
  | PFloat f   -> [%expr Camlrack.PFloat [%e efloat ~loc (string_of_float f)]]
  | PString s  -> [%expr Camlrack.PString [%e estring ~loc s]]
  | PSymbol s  -> [%expr Camlrack.PSymbol [%e estring ~loc s]]
  | SPat pats  -> [%expr Camlrack.SPat [%e elist ~loc (List.map (sexp_pattern_exp_of_sexp_pattern ~loc) pats)]]

let convert_simple_pat
    ~loc
    ~rhs
    (fresh_groupname : unit -> string)
    (sexp_pat : sexp_pattern) : case list =
  let open Ast_builder.Default in
  let simple_case (pat : pattern) : case list = [case ~lhs:pat ~guard:None ~rhs] in
  match sexp_pat with
  | SYMBOL     -> simple_case [%pat? Camlrack.Symbol [%p ppat_any ~loc]]
  | INTEGER    -> simple_case [%pat? Camlrack.Integer [%p ppat_any ~loc]]
  | FLOAT      -> simple_case [%pat? Camlrack.Float [%p ppat_any ~loc]]
  | STRING     -> simple_case [%pat? Camlrack.String [%p ppat_any ~loc]]
  | ANY        -> simple_case (ppat_any ~loc)
  | PInteger i -> simple_case [%pat? Camlrack.Integer [%p pint ~loc i]]
  | PFloat f   -> simple_case [%pat? Camlrack.Float [%p pfloat ~loc (string_of_float f)]]
  | PString s  -> simple_case [%pat? Camlrack.String [%p pstring ~loc s]]
  | PSymbol s  -> simple_case [%pat? Camlrack.Symbol [%p pstring ~loc s]]
  (* TODO: Implement more sophisticated handling of the sub-pattern case. *)
  | SPat _     ->
    let groupname = fresh_groupname () in
    let pat = pvar ~loc groupname in
    let guard = Some [%expr (Camlrack.sexp_match
                               [%e sexp_pattern_exp_of_sexp_pattern ~loc sexp_pat]
                               [%e evar ~loc groupname])] in
    [case ~lhs:pat ~guard ~rhs]

let convert_case (c : case) : (case list) option =
  let groupno = ref 0 in
  let freshno () = groupno := !groupno + 1; !groupno in
  let fresh_groupname () = "subgroup" ^ string_of_int (freshno ()) in
  match c.pc_lhs.ppat_desc with
  (* Wildcards are left alone. *)
  | Ppat_any -> Some [c]
  (* Constant strings are parsed and converted. *)
  | Ppat_constant (Pconst_string (s, loc, _)) ->
    (Option.join (Camlrack.sexp_of_string_opt s <&> Camlrack.sexp_pattern_of_sexp))
    <&> convert_simple_pat ~loc ~rhs:c.pc_rhs fresh_groupname
  (* Anything else results in a failed conversion. *)
  (* TODO: Maybe allow other things? Assume the programmer is correct? *)
  | _ -> None

let process_scrutinee (e : expression) : expression =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (_, _, _)) ->
    (match sexp_exp_of_expression e with
     | Some ne -> ne
     | None -> e)
  | _ -> e

let expand_spat ~ctxt (e : expression) : expression =
  let open Ast_builder.Default in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match e.pexp_desc with
  | Pexp_match (scrut, cases) ->
    (* Only replace the match-expression if we are successful. *)
    let converted_cases = List.map convert_case cases in
    if List.for_all Option.is_some converted_cases
    then
      (let processed_scrut = process_scrutinee scrut in
       let cases' = List.map Option.get converted_cases in
       let new_match = pexp_match ~loc:e.pexp_loc (evar ~loc scrutinee_match_name) (List.flatten cases') in
       pexp_let
         ~loc
         Nonrecursive
         [{ pvb_pat = pvar ~loc scrutinee_match_name
          ; pvb_expr = processed_scrut
          ; pvb_attributes = []
          ; pvb_loc = loc }]
         new_match)
    else e
  | _ -> e

let ppx_spat =
  Extension.V3.declare
    "spat"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_spat

let spat_rule = Context_free.Rule.extension ppx_spat
