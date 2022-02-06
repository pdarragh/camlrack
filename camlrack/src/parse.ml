open Errors
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

type parse_error =
  | EmptySExpression
  | MultipleSExpressions
  | UnterminatedSExpression of string
  | UnexpectedClosingBrace of string
  | MismatchedBraces of string * string

let render_error (pe : parse_error) : string =
  match pe with
  | EmptySExpression -> "no S-Expression was given"
  | MultipleSExpressions -> "multiple S-Expressions were found"
  | UnterminatedSExpression brace -> "an opening brace was never terminated: " ^ brace
  | UnexpectedClosingBrace brace -> "an unmatched closing brace encountered: " ^ brace
  | MismatchedBraces (l, r) -> "these braces do not match: " ^ l ^ " and " ^ r

let parse_tokens_to_many (tokens : token list) : (sexp list, parse_error) result =
  let rec parse' (tokens : token list) (braces : token list) (stack : sexp list list) : (sexp list, parse_error) result =
    match tokens with
    | [] ->
      (match braces with
       | [] ->
         (match stack with
          | ss::ss'::stack -> parse' tokens braces ((ss' @ [SExp ss])::stack)
          | _ ->
            let results = List.flatten stack in
            match results with
            | [] -> Error EmptySExpression
            | _ -> Ok results)
       | brace::_ -> Error (UnterminatedSExpression (string_of_token brace)))
    | token::tokens ->
      (match token with
       | LParen | LBrace | LBracket -> parse' tokens (token::braces) ([]::stack)
       | RParen | RBrace | RBracket ->
         (match braces with
          | [] -> Error (UnexpectedClosingBrace (string_of_token token))
          | brace::braces when braces_match brace token ->
            parse' tokens braces (((List.hd (List.tl stack)) @ [SExp (List.hd stack)]) :: (List.tl (List.tl stack)))
          | brace::_ ->
            Error (MismatchedBraces (string_of_token brace, string_of_token token)))
       | Integer i -> parse' tokens braces ((List.hd stack @ [Integer i]) :: (List.tl stack))
       | Float f -> parse' tokens braces ((List.hd stack @ [Float f]) :: (List.tl stack))
       | String s -> parse' tokens braces ((List.hd stack @ [String s]) :: (List.tl stack))
       | Symbol s -> parse' tokens braces ((List.hd stack @ [Symbol s]) :: (List.tl stack)))
  in
  parse' tokens [] [[]]

let parse_tokens_to_one (tokens : token list) : (sexp, parse_error) result =
  match parse_tokens_to_many tokens with
  | Ok [result] -> Ok result
  | Ok _ -> Error MultipleSExpressions
  | Error e -> Error e

let parse (s : string) : (sexp, parse_error) result = tokenize s |> parse_tokens_to_one

let parse_exn (s : string) : sexp =
  match parse s with
  | Ok s -> s
  | Error e -> raise (CamlrackError ("A list of S-Expressions could not be parsed because " ^ render_error e))

let parse_opt (s : string) : sexp option =
  match parse s with
  | Ok s -> Some s
  | Error _ -> None

let parse_many (s : string) : (sexp list, parse_error) result = tokenize s |> parse_tokens_to_many

let parse_many_exn (s : string) : sexp list =
  match parse_many s with
  | Ok s -> s
  | Error e -> raise (CamlrackError ("A single S-Expression could not be parsed because " ^ render_error e))

let parse_many_opt (s : string) : (sexp list) option =
  match parse_many s with
  | Ok s -> Some s
  | Error _ -> None
