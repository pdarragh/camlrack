open TokenTypes

let regexps =
  [ (Str.regexp "[[({]", fun _ -> LParen)
  ; (Str.regexp "[])}]", fun _ -> RParen)
  ; (Str.regexp "-?[0-9]+", (fun s -> Integer (int_of_string s)))
  ; (Str.regexp "[^])}[({ \t\n0-9][^])}[({ \t\n]*", (fun s -> Symbol s))
  ]

let whitespace = Str.regexp "[ \t\n]*"

let tokenize (input : string) : token list =
  let length = String.length input in
  let pos = ref 0 in
  let advance (offset : int) =
    pos := !pos + offset in
  let spaces () =
    let _ = Str.string_match whitespace input !pos in
    advance (String.length (Str.matched_string input)) in
  let try_regexp (regexp, confun) : (int * token) option =
    if Str.string_match regexp input !pos
    then
      let s = Str.matched_string input in
      Some (String.length s, confun s)
    else None in
  let tokens = ref [] in
  while !pos < length do
    spaces ();
    let matches = List.filter_map try_regexp regexps in
    let (length, token) = List.fold_left
        (fun p c -> if fst c > fst p then c else p)
        (-1, LParen) matches in
    if length < 0
    then failwith ("Could not lex input at position " ^ string_of_int !pos ^
                   ", beginning with character: " ^ String.sub input !pos 1)
    else (tokens := token::!tokens; advance length)
  done;
  List.rev !tokens
