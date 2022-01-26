open Common
open Camlrack.Match
open Ppxlib

let ( <&> ) (x : 'a option) (f : 'a -> 'b) : 'b option = Option.map f x

let replicate (n : int) (x : 'a) : 'a list =
  let rec replicate' (n : int) (xs : 'a list) : 'a list =
    if n = 0
    then xs
    else replicate' (n - 1) (x :: xs)
  in
  replicate' n []

let scrutinee_match_name = "sexp"

type conditional_pattern = pattern * (expression list)

let uncond_pat (pat : pattern) : conditional_pattern = (pat, [])

let rec sexp_pattern_exp_of_sexp_pattern ~loc (spat : sexp_pattern) : expression =
  let open Ast_builder.Default in
  match spat with
  | SYMBOL        -> [%expr Camlrack.SYMBOL]
  | INTEGER       -> [%expr Camlrack.INTEGER]
  | FLOAT         -> [%expr Camlrack.FLOAT]
  | STRING        -> [%expr Camlrack.STRING]
  | ANY           -> [%expr Camlrack.ANY]
  | PInteger i    -> [%expr Camlrack.PInteger [%e eint ~loc i]]
  | PFloat   f    -> [%expr Camlrack.PFloat [%e efloat ~loc (string_of_float f)]]
  | PString  s    -> [%expr Camlrack.PString [%e estring ~loc s]]
  | PSymbol  s    -> [%expr Camlrack.PSymbol [%e estring ~loc s]]
  | SPat     pats -> [%expr Camlrack.SPat [%e elist ~loc (List.map (sexp_pattern_exp_of_sexp_pattern ~loc) pats)]]

(* Produces a list of patterns corresponding to 0-`len` repetitions of the given
   pattern. *)
let rec replicate_pattern
    (pat : pattern)
    (pats : pattern list list)
    (len : int) : pattern list list =
  if len = 0
  then
    (* Add the empty list for zero matches. *)
    [] :: pats
  else
    (* Add the pattern repeated `len` times. *)
    let sexp_pat = (replicate len pat) in
    replicate_pattern pat (sexp_pat :: pats) (len - 1)

(* The number of times to expand a subgroup before using a variable match. *)
let subgroup_repeat_count = 2

(* Given a pattern that is meant to repeat (i.e., via a "..." symbol), produces
   the needed repetitions of that pattern. *)
let build_replicated_patterns
    ~loc
    (fresh_groupname : (unit -> label))
    (sexp_pat : sexp_pattern)
    (pat : pattern)
    (conds : expression list)
    (add_var_cpat : bool) : (pattern list * expression list) list =
  let open Ast_builder.Default in
  let replicated_patterns = replicate_pattern pat [] subgroup_repeat_count in
  let fixed_cpats = List.map (fun p -> (p, conds)) replicated_patterns in
  let groupname = fresh_groupname () in (* FIXME: move into then-body *)
  if add_var_cpat
  then
    let var_cpat = ([[%pat? Camlrack.SExp [%p pvar ~loc groupname]]],
                    [%expr (List.for_all
                              (Camlrack.sexp_match
                                 [%e sexp_pattern_exp_of_sexp_pattern ~loc sexp_pat])
                              [%e evar ~loc groupname])]
                    :: conds) in
    List.append fixed_cpats [var_cpat]
  else fixed_cpats

(* Constructs a guard for the revised match case. *)
let build_guard ~loc (c : case) (conds : expression list) : expression option =
  let rec and_conds (conds : expression list) : expression =
    match conds with
    | [] -> failwith "invalid" (* FIXME *)
    | cond :: [] -> cond
    | cond :: conds' -> [%expr [%e cond] && [%e and_conds conds']] in
  match conds with
  | [] -> c.pc_guard
  | _ ->
    let anded_conds = and_conds conds in
    (match c.pc_guard with
     | Some g -> Some [%expr [%e g] && [%e anded_conds]]
     | None -> Some anded_conds)

let convert_case (c : case) : (case list) option =
  let open Ast_builder.Default in
  let convert_sexp_pattern ~loc (pat : sexp_pattern) : conditional_pattern list =
    (* Sub-group names are tracked throughout an entire case conversion. *)
    let groupno = ref 0 in
    let freshno () = groupno := !groupno + 1; !groupno in
    let fresh_groupname () =
      let group = freshno () in
      "subgroup" ^ string_of_int group in
    (* Returns a list of conditional patterns. Each pattern should be converted
       into a single case in the match expression with the associated conditions
       added to the guard. *)
    let rec convert_pattern (pat : sexp_pattern) (add_var_cpat : bool) : conditional_pattern list =
      match pat with
      | SYMBOL        -> [uncond_pat [%pat? Camlrack.Symbol [%p ppat_any ~loc]]]
      | INTEGER       -> [uncond_pat [%pat? Camlrack.Integer [%p ppat_any ~loc]]]
      | FLOAT         -> [uncond_pat [%pat? Camlrack.Float [%p ppat_any ~loc]]]
      | STRING        -> [uncond_pat [%pat? Camlrack.String [%p ppat_any ~loc]]]
      | ANY           -> [uncond_pat (ppat_any ~loc)]
      | PInteger i    -> [uncond_pat [%pat? Camlrack.Integer [%p pint ~loc i]]]
      | PFloat   f    -> [uncond_pat [%pat? Camlrack.Float [%p pfloat ~loc (string_of_float f)]]]
      | PString  s    -> [uncond_pat [%pat? Camlrack.String [%p pstring ~loc s]]]
      | PSymbol  s    -> [uncond_pat [%pat? Camlrack.Symbol [%p pstring ~loc s]]]
      | SPat     pats -> (convert_subpats pats add_var_cpat)
    (* Converts a list of patterns corresponding to list-like S-Expressions into
       a list of conditional patterns. *)
    and convert_subpats (pats : sexp_pattern list) (add_var_cpat : bool) : conditional_pattern list =
      let rec convert_subpats'
          (prevs : (pattern list * expression list) list)
          (sexp_pats : sexp_pattern list) =
        match sexp_pats with
        (* When no sub-patterns remain, produce a set of S-Expressions. *)
        | [] ->
          List.map
            (fun (pats, conds) ->
               ([%pat? Camlrack.SExp [%p plist ~loc (List.rev pats)]], conds))
            prevs
        (* When there is an ellipsis, create the desired repeat patterns. *)
        | sexp_pat :: PSymbol "..." :: sexp_pats' ->
          let pat_expansions = convert_pattern sexp_pat false in
          let replicated_expansions : (pattern list * expression list) list =
            List.flatten
              (List.map
                 (fun (p, conds) -> build_replicated_patterns ~loc fresh_groupname sexp_pat p conds add_var_cpat)
                 pat_expansions) in
          let new_prevs : (pattern list * expression list) list =
            List.flatten
              (List.map
                 (fun (new_pats, new_conds) ->
                    (* When there are no elements yet in the list, we must
                       populate it manually. *)
                    if prevs = []
                    then [(new_pats, new_conds)]
                    else
                      (List.map
                         (fun (prev_pats, prev_conds) ->
                            ((List.append new_pats prev_pats),
                             (List.append new_conds prev_conds)))
                         prevs))
                 replicated_expansions) in
          convert_subpats' new_prevs sexp_pats'
        (* In the other case, convert the next expression and prepend it to the
           list of processed sub-patterns. *)
        | sexp_pat :: sexp_pats' ->
          let pat_expansions = convert_pattern sexp_pat add_var_cpat in
          let new_prevs =
            List.flatten
              (List.map
                 (fun (new_pat, new_conds) ->
                    (* When there are no elements yet in the list, we must
                       populate it manually. *)
                    if prevs = []
                    then [([new_pat], new_conds)]
                    else
                      List.map
                        (fun (prev_pats, prev_conds) ->
                           (new_pat :: prev_pats,
                            List.append new_conds prev_conds))
                        prevs)
                 pat_expansions) in
          convert_subpats' new_prevs sexp_pats' in
      let converted_subpats = convert_subpats' [] pats in
      if converted_subpats = []
      then [uncond_pat [%pat? Camlrack.SExp []]]
      else converted_subpats in
    convert_pattern pat true in
  let convert_cond_pat_to_cases ~loc ((pat, conds) : conditional_pattern) : case =
    case ~lhs:pat ~guard:(build_guard ~loc c conds) ~rhs:c.pc_rhs
  in
  match c.pc_lhs.ppat_desc with
  | Ppat_any -> Some [c]
  | Ppat_constant (Pconst_string (s, loc, _)) ->
    (Option.join (Camlrack.sexp_of_string s
                  <&> Camlrack.sexp_pattern_of_sexp))
    <&> (convert_sexp_pattern ~loc)
    <&> (List.map (convert_cond_pat_to_cases ~loc:c.pc_lhs.ppat_loc))
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
