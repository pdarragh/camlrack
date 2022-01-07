open Ppxlib
open Ppx_sexp
open Ppx_spat

let () = Driver.register_transformation ~rules:[ sexp_rule; spat_rule ] "ppx_camlrack"
