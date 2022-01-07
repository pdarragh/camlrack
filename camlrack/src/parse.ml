open Tokenize

include Sexp

let push (buffer : 'a list ref) (elem : 'a) : unit =
  buffer := elem::!buffer

let pop (buffer : 'a list ref) : 'a =
  let head = List.hd !buffer in buffer := List.tl !buffer; head

let matching_braces =
  [ (LParen, RParen)
  ; (LBrace, RBrace)
  ; (LBracket, RBracket)
  ]

let braces_match (lb : token) (rb : token) : bool =
  List.mem (lb, rb) matching_braces

let string_of_sexp_list (sexps : sexp list) : string =
  "[" ^ String.concat "; " (List.map render_string_of_sexp sexps) ^ "]"

let string_of_sexp_list_list (sexpss : sexp list list) : string =
  "[" ^ String.concat "; " (List.map string_of_sexp_list sexpss) ^ "]"

let parse_tokens_multiple (tokens : token list) : sexp list =
  let rec parse' (tokens : token list) (braces : token list) (stack : sexp list list) : sexp list =
    match tokens with
    | [] ->
      (match braces with
       | [] ->
         (match stack with
          | ss::ss'::stack -> parse' tokens braces ((ss' @ [SExp ss])::stack)
          | _ -> List.flatten stack)
       | _ -> failwith "unterminated S-expression")
    | token::tokens ->
      (match token with
       | LParen | LBrace | LBracket -> parse' tokens (token::braces) ([]::stack)
       | RParen | RBrace | RBracket ->
         (match braces with
          | [] -> failwith "unexpected closing brace"
          | brace::braces when braces_match brace token ->
            parse' tokens braces (((List.hd (List.tl stack)) @ [SExp (List.hd stack)]) :: (List.tl (List.tl stack)))
          | _ -> failwith "mismatched braces")
       | Integer i -> parse' tokens braces ((List.hd stack @ [Integer i]) :: (List.tl stack))
       | Float f -> parse' tokens braces ((List.hd stack @ [Float f]) :: (List.tl stack))
       | String s -> parse' tokens braces ((List.hd stack @ [String s]) :: (List.tl stack))
       | Symbol s -> parse' tokens braces ((List.hd stack @ [Symbol s]) :: (List.tl stack)))
  in
  parse' tokens [] [[]]

let parse_tokens (tokens : token list) : sexp option =
  match parse_tokens_multiple tokens with
  | [] -> None
  | [s] -> Some s
  | _   -> failwith "found multiple parses"

let parse_tokens_exn (tokens : token list) : sexp =
  match parse_tokens tokens with
  | Some s -> s
  | None -> failwith "could not parse S-expression"

let parse (s : string) : sexp option = tokenize s |> parse_tokens

let parse_exn (s : string) : sexp = tokenize s |> parse_tokens_exn
