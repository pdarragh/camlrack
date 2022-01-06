include Sexp
include SexpPatterns

let sexp_match (p : sexp_pattern) (se : sexp) : bool =
  let rec sexp_match (p : sexp_pattern) (se : sexp) : bool =
    (* print_endline ("matching pattern " ^ string_of_sexp_pattern p ^ " with S-expression " ^ string_of_sexp se); *)
    match p, se with
    | SYMBOL, Symbol _ -> true
    | NUMBER, Integer _ -> true
    | ANY, _ -> true
    | Symbol s1, Symbol s2 -> s1 = s2
    | Number i1, Integer i2 -> i1 = i2
    | SPat pats, Sexp sexps -> list_match pats sexps
    | _ -> false
  and list_match (ps : sexp_pattern list) (ses : sexp list) : bool =
    match ps, ses with
    | [], [] -> true
    | [], _ -> false
    | p :: Symbol "..." :: ps', _ ->
      if ((List.length ps) - 2) = List.length ses
      then list_match ps' ses
      else if ses = []
      then false
      else sexp_match p (List.hd ses) && list_match ps (List.tl ses)
    | p :: ps', se :: ses' -> sexp_match p se && list_match ps' ses'
    | _ -> false
  in sexp_match p se
