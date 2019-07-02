(* Knuth-Bendix 完備化 *)
open Trs

type precedence = (funsym * int) list
type lexpathorder = term -> term -> bool

exception CompletionFailed

val unify : term -> term -> substitutionset option
val crpair : rule -> rule -> equationset
val lpogreq : precedence -> lexpathorder
val lpogr : precedence -> lexpathorder

val kbc : precedence -> equationset -> ruleset
val kbcv : precedence -> equationset -> ruleset
val kbcf : lexpathorder -> equationset -> ruleset
val kbcfv : lexpathorder -> equationset -> ruleset
