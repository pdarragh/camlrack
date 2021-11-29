open Sexp
open Tokenize.TokenTypes

module Sexp = Sexp

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
  "[" ^ String.concat "; " (List.map string_of_sexp sexps) ^ "]"

let string_of_sexp_list_list (sexpss : sexp list list) : string =
  "[" ^ String.concat "; " (List.map string_of_sexp_list sexpss) ^ "]"

let parse (tokens : token list) : sexp list option =
  let rec parse' (tokens : token list) (braces : token list) (stack : sexp list list) : sexp list option =
    match tokens with
    | [] ->
      (match braces with
       | [] ->
         (match stack with
          | [] -> None
          | [ss] -> Some ss
          | ss::ss'::stack -> parse' tokens braces ((ss' @ [Sexp ss])::stack))
       | _ -> failwith "unterminated s-expression")
    | token::tokens ->
      (match token with
       | LParen | LBrace | LBracket -> parse' tokens (token::braces) ([]::stack)
       | RParen | RBrace | RBracket ->
         (match braces with
          | [] -> failwith "unexpected closing brace"
          | brace::braces when braces_match brace token ->
            parse' tokens braces (((List.hd (List.tl stack)) @ [Sexp (List.hd stack)]) :: (List.tl (List.tl stack)))
          | _ -> failwith "mismatched braces")
       | Integer i -> parse' tokens braces ((List.hd stack @ [Integer i]) :: (List.tl stack))
       | Symbol s -> parse' tokens braces ((List.hd stack @ [Symbol s]) :: (List.tl stack)))
  in
  parse' tokens [] [[]]
