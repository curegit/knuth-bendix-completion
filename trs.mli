type funsym = string
type varsym = string * int
type term = Variable of varsym | Function of (funsym * term list)
type rule = term * term
type equation = term * term
type substitution = varsym * term
type ruleset = rule list
type equationset = equation list
type substitutionset = substitution list

exception ParseError

val height : term -> int
val leaves : term -> int
val nodes : term -> int
val vars : term -> varsym list
val funs : term -> funsym list
val subst : substitutionset -> term -> term
val collate : term -> term -> substitutionset option
val contain : term -> term -> bool
val rewrite : ruleset -> term -> term option

val lireduce : ruleset -> term -> term option
val linorm : ruleset -> term -> term
val loreduce : ruleset -> term -> term option
val lonorm : ruleset -> term -> term
val poreduce : ruleset -> term -> term option
val ponorm : ruleset -> term -> term

val rename : varsym * varsym -> term -> term
val uniquevar : rule * rule -> rule * rule
val decvarsub : rule -> rule
val sameeq : equation -> equation -> bool
val distincteqs : equationset -> equationset

val var : string -> term
val const : string -> term
val func : string -> string list -> term
val call : string -> term list -> term
val nest : string -> int -> term -> term

val parseterm : string -> term
val parsevar : string -> term
val parsefun : string -> term
val parserule : string -> rule
val parseeq : string -> equation
val parserules : string list -> ruleset
val parseeqs : string list -> equationset

val strterm : term -> string
val strrule : rule -> string
val streq : equation -> string
val strrules : ruleset -> string
val streqs : equationset -> string

val printterm : term -> unit
val printrule : rule -> unit
val printeq : equation -> unit
val printrules : ruleset -> unit
val printeqs : equationset -> unit
