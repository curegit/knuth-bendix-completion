(* 項書換え系 *)

module type TermRewritingSystemSignature = sig

  type term = Function of (string * term list) | Variable of (string * int)
  type rule = (term * term)
  type substitution = ((string * int) * term)

  val depth : term -> int
  val vars : term -> (string * int) list
  val subst : substitution list -> term -> term
  val collate : term -> term -> substitution list option
  val rewrite : (term * term) list -> term -> term option
(*
  val linorm : (term * term) list -> term -> term
  val liredux : (term * term) list -> term -> term option
  val lonorm : (term * term) list -> term -> term
  val loredux : (term * term) list -> term -> term option
  val ponorm : (term * term) list -> term -> term
  val poredux : (term * term) list -> term -> term option
*)
(*
  val parseterm : string -> term
  val printterm : term -> string
*)
end

module TermRewritingSystem : TermRewritingSystemSignature = struct

  open TermRewritingSystemUtility

  type term = Function of (string * term list) | Variable of (string * int)
  type rule = (term * term)
  type substitution = ((string * int) * term)

  let rec depth = function
                  | Variable xi -> 1
                  | Function (f, ts) -> 1 + depthlist ts
  and depthlist = function
                  | [] -> 0
                  | t :: ts -> max (depth t) (depthlist ts)

  let rec vars = function
                 | Variable xi -> []
                 | Function (f, ts) -> varslist ts
  and varslist = function
                 | [] -> []
                 | t :: ts -> union (vars t) (varslist ts)

  let rec subst ss = function
                     | Variable xi -> (match find xi ss with
                                       | Some s -> s
                                       | None -> Variable xi)
                     | Function (f, ts) -> Function (f, map (subst ss) ts)

  let rec collate l r = match l with
                        | Variable xi -> Some [(xi, r)]
                        | Function (f, ts) -> match r with
                                              | Variable xi' -> None
                                              | Function (f', ts') -> if f = f' then collatelist ts ts' else None
  and collatelist ls rs = match (ls, rs) with
                          | ([], []) -> Some []
                          | (t :: ts, t' :: ts') -> (match (collate t t', collatelist ts ts') with
                                                     | (Some s, Some s') -> append s s'
                                                     | (_, _) -> None)
                          | (_, _) -> None

  let rec rewrite rs t = match rs with
                         | [] -> None
                         | (l, r) :: rs -> match collate l t with
                                           | Some s -> Some (subst s r)
                                           | None -> rewrite rs t
(*
  let rec linorm
  let rec liredux
  let rec lonorm
  let rec loredux
  let rec ponorm
  let rec poredux
*)
end

