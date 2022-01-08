include Sexp
include SexpPatterns

module ListConvenienceFunctions = ListConvenience

(* The implementation of this function and the associated `sexp_pattern` type
   are heavily inspired (almost exactly copied) from the implementation of the
   `s-exp-match?` function in the PLAIT language by Matthew Flatt:

   https://github.com/mflatt/plait/blob/8d842132a11ab7c7621c428c609dcddc2769d145/main.rkt#L238-L340
*)
let sexp_match (p : sexp_pattern) (se : sexp) : bool =
  let rec sexp_match (p : sexp_pattern) (se : sexp) : bool =
    match p, se with
    | SYMBOL, Symbol _ -> true
    | INTEGER, Integer _ -> true
    | FLOAT, Float _ -> true
    | STRING, String _ -> true
    | ANY, _ -> true
    | PSymbol s1, Symbol s2 -> s1 = s2
    | PInteger i1, Integer i2 -> i1 = i2
    | PFloat f1, Float f2 -> f1 = f2
    | PString s1, String s2 -> s1 = s2
    | SPat pats, SExp sexps -> list_match pats sexps
    | _ -> false
  and list_match (ps : sexp_pattern list) (ses : sexp list) : bool =
    match ps, ses with
    | [], [] -> true
    | [], _ -> false
    | p :: PSymbol "..." :: ps', _ ->
      if ((List.length ps) - 2) = List.length ses
      then list_match ps' ses
      else if ses = []
      then false
      else sexp_match p (List.hd ses) && list_match ps (List.tl ses)
    | p :: ps', se :: ses' -> sexp_match p se && list_match ps' ses'
    | _ -> false
  in sexp_match p se
