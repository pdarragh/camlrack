# Camlrack API

This document provides information about all of the types and functions exported
by Camlrack.


## Types


### S-Expressions

  * <a id="sexp" /> `sexp`: S-Expressions are composed of...
      * <a id="Integer" /> `Integer` of `int`: Integer literals.
      * <a id="Float" /> `Float` of `float`: Floating-point literals.
      * <a id="String" /> `String` of `string`: String literals.
      * <a id="Symbol" /> `Symbol` of `string`: Symbols. (Not quite the same as
        [Strings](#String).)
      * <a id="sexp-SExp" /> `SExp` of [`sexp`](#sexp) `list`: Sub-lists of
        S-Expressions.


### Patterns for Matching S-Expressions

Sometimes one does not wish to match S-Expressions literally, but would instead
prefer a regex-like experience for matching against *shapes* of S-Expressions.
The `sexp_pattern` type is used for specifying such shapes:

  * <a id="sexp_pattern" /> `sexp_pattern` : S-Expression matching patterns are
    composed of...
      * <a id="SYMBOL" /> `SYMBOL` : a [`Symbol`](#Symbol) wildcard.
      * <a id="INTEGER" /> `INTEGER` : an [`Integer`](#Integer) wildcard.
      * <a id="FLOAT" /> `FLOAT` : a [`Float`](#Float) wildcard.
      * <a id="STRING" /> `STRING` : a [`String`](#String) wildcard.
      * <a id="ANY" /> `ANY` : an [`sexp`](#sexp) wildcard.
      * <a id="PInteger" /> `PInteger` of `int` : an exact match for an
        [`Integer`](#Integer).
      * <a id="PFloat" /> `PFloat` of `float` : an exact match for a
        [`Float`](#Float).
      * <a id="PString" /> `PString` of `string` : an exact match for a
        [`String`](#String).
      * <a id="PSymbol" /> `PSymbol` of `string` : an exact match for a
        [`Symbol`](#Symbol).
      * <a id="SPat" /> `SPat` of [`sexp_pattern`](#sexp_pattern) `list` : an
        exact match for a sub-list of [`sexp`s](#sexp).


## Functions

### Parsing Strings to S-Expressions

  * <a id="sexp_of_string" /> `sexp_of_string` : `string` &rarr; [`sexp`](#sexp)
    `option`

    Attempts to convert a string to an [`sexp`](#sexp)

  * <a id="sexp_of_string_exn" /> `sexp_of_string_exn` : `string` &rarr;
    [`sexp`](#sexp)

    Converts a string to an [`sexp`](#sexp), or else raises an exception.


### Converting Whole S-Expressions to Strings

  * <a id="render_string_of_sexp" /> `render_string_of_sexp` : [`sexp`](#sexp)
    &rarr; `string`

    Converts an [`sexp`](#sexp) to a string representing that S-Expression. For
    example:

    ```ocaml
    # render_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "(foo 42)"
    ```

  * <a id ="repr_string_of_sexp" /> `repr_string_of_sexp` : [`sexp`](#sexp)
    &rarr; `string`

    Converts an [`sexp`](#sexp) to a string that is explicitly annotated, useful
    for debugging but not much else. For example:

    ```ocaml
    # repr_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "SExp [Symbol \"foo\"; Integer 42]"
    ```


### Other S-Expression Conversion Functions

  * <a id ="sexp_to_list_opt" /> `sexp_to_list_opt` : [`sexp`](#sexp) &rarr;
    ([`sexp`](#sexp) `list`) `option`

    Given an [`SExp`](#sexp-SExp) variant of [`sexp`](#sexp), returns a `Some`
    containing the internal list of S-Expressions. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_list_opt (SExp [Integer 1; Integer 2]);;
    - : sexp list option = Some [Integer 1; Integer 2]
    # sexp_to_list_opt (Integer 3);;
    - : sexp list option = None
    ```

  * <a id ="sexp_to_list" /> `sexp_to_list` : [`sexp`](#sexp) &rarr;
    [`sexp`](#sexp) `list`

    Given an [`SExp`](#sexp-SExp) variant of [`sexp`](#sexp), returns the
    internal list of S-Expressions. Otherwise, raises an exception.

    ```ocaml
    # sexp_to_list (SExp [Integer 1; Integer 2]);;
    - : sexp list option = [Integer 1; Integer 2]
    # sexp_to_list (Integer 3);;
    Exception: Failure "S-Expression is not a list: 3".
    ```

  * <a id="list_to_sexp" /> `list_to_sexp` : [`sexp`](#sexp) `list` &rarr;
    [`sexp`](#sexp)

    Given a list of [`sexp`](#sexp)s, produces a single [`SExp`](#sexp-SExp).

    ```ocaml
    # list_to_sexp [Integer 1; Integer 2];;
    - : sexp = SExp [Integer 1; Integer 2]
    ```

  * <a id="sexp_to_int_opt" /> `sexp_to_int_opt` : [`sexp`](#sexp) &rarr; `int`
    `option`

    Given an [`Integer`](#Integer), returns a `Some` containing the internal
    `int`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_int_opt (Integer 1);;
    - : int option = Some 1
    # sexp_to_int_opt (String "not an integer");;
    - : int option = None
    ```

  * <a id="sexp_to_int" /> `sexp_to_int` : [`sexp`](#sexp) &rarr; `int`

    Given an [`Integer`](#Integer), returns the internal `int`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_int (Integer 1);;
    - : int = 1
    # sexp_to_int (String "not an integer");;
    Exception: Failure "S-Expression is not an integer: \"not an integer\"".
    ```

  * <a id="int_to_sexp" /> `int_to_sexp` : `int` &rarr; [`sexp`](#sexp)

    Given an `int`, produces an [`Integer`](#Integer).

    ```ocaml
    # int_to_sexp 42;;
    - : sexp = Integer 42
    ```

  * <a id="sexp_to_float_opt" /> `sexp_to_float_opt` : [`sexp`](#sexp) &rarr;
    `float` `option`

    Given a [`Float`](#Float), returns a `Some` containing the internal `float`.
    Otherwise, returns `None`.

    ```ocaml
    # sexp_to_float_opt (Float 1.0);;
    - : float option = Some 1.0
    # sexp_to_float_opt (String "not a float");;
    - : float option = None
    ```

  * <a id="sexp_to_float" /> `sexp_to_float` : [`sexp`](#sexp) &rarr; `float`

    Given a [`Float`](#Float), returns the internal `float`. Otherwise, raises
    an exception.

    ```ocaml
    # sexp_to_float (Float 1.0);;
    - : float = 1.0
    # sexp_to_float (String "not a float");;
    Exception: Failure "S-Expression is not a float: \"not a float\"".
    ```

  * <a id="float_to_sexp" /> `float_to_sexp` : `float` &rarr; [`sexp`](#sexp)

    Given a `float`, produces a [`Float`](#Float).

    ```ocaml
    # float_to_sexp 4.2;;
    - : sexp = Float 4.2
    ```

  * <a id="sexp_to_string_opt" /> `sexp_to_string_opt` : [`sexp`](#sexp) &rarr;
    `string` `option`

    Given a [`String`](#String), returns a `Some` containing the internal
    `string`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_string_opt (String "hello");;
    - : string option = Some "hello"
    # sexp_to_string_opt (Integer 1);;
    - : string option = None
    ```

  * <a id="sexp_to_string" /> `sexp_to_string` : [`sexp`](#sexp) &rarr; `string`

    Given a [`String`](#String), returns the internal `string`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_string (String "hello");;
    - : string = "hello"
    # sexp_to_string (Integer 1);;
    Exception: Failure "S-Expression is not a string: 1".
    ```

  * <a id="string_to_sexp" /> `string_to_sexp` : `string` &rarr; [`sexp`](#sexp)

    Given a `string`, produces a [`String`](#String).

    ```ocaml
    # string_to_sexp "some string";;
    - : sexp = String "some string"
    ```

  * <a id="sexp_to_symbol_opt" /> `sexp_to_symbol_opt` : [`sexp`](#sexp) &rarr;
    `string` `option`

    Given a [`Symbol`](#Symbol), returns a `Some` containing the internal
    `string`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_symbol_opt (Symbol "hello");;
    - : string option = Some "hello"
    # sexp_to_symbol_opt (Integer 1);;
    - : string option = None
    ```

  * <a id="sexp_to_symbol" /> `sexp_to_symbol` : [`sexp`](#sexp) &rarr; `string`

    Given a [`Symbol`](#Symbol), returns the internal `string`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_symbol (Symbol "hello");;
    - : string = "hello"
    # sexp_to_string (Integer 1);;
    Exception: Failure "S-Expression is not a string: 1".
    ```

  * <a id="symbol_to_sexp" /> `symbol_to_sexp` : `string` &rarr; [`sexp`](#sexp)

    Given a `string`, produces a [`Symbol`](#Symbol).

    ```ocaml
    # symbol_to_sexp "foo";;
    - : sexp = Symbol "foo"
    ```
