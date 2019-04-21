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
  val lireduce : (term * term) list -> term -> term option
  val linorm : (term * term) list -> term -> term
  val loreduce : (term * term) list -> term -> term option
  val lonorm : (term * term) list -> term -> term
*)
  val poreduce : (term * term) list -> term -> term option
  val ponorm : (term * term) list -> term -> term

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
  let rec lireduce
  let rec linorm
  let rec loreduce
  let rec lonorm
*)
  let rec poreduce rs = function
                        | Variable xi -> rewrite rs (Variable xi)
                        | Function (f, ts) -> match rewrite rs (Function (f, ts)) with
                                              | Some t -> Some t
                                              | None -> match poreducelist rs ts with
                                                        | Some ts' -> Some (Function (f, ts'))
                                                        | None -> None
  and poreducelist rs = function
                        | [] -> None
                        | t :: ts -> match (poreduce rs t, poreducelist rs ts) with
                                     | (Some t', Some ts') -> Some (t' :: ts')
                                     | (Some t', None) -> Some (t' :: ts)
                                     | (None, Some ts') -> Some (t :: ts')
                                     | (None, None) -> None

  let rec ponorm rs t = match poreduce rs t with
                        | Some t' -> ponorm rs t'
                        | None -> t

end
