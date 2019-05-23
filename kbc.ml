(* Knuth-Bendix 完備化 *)

module type KnuthBendixCompletionSignature = sig

  open TermRewritingSystem

  type precedence = (funsym * int) list

  val unify : term -> term -> substitutionset option
  val crpair : rule -> rule -> equationset
  val lpogreq : precedence -> term -> term -> bool
  val lpogr : precedence -> term -> term -> bool

end

module KnuthBendixCompletion : KnuthBendixCompletionSignature = struct

  open Utility
  open TermRewritingSystem

  type precedence = (funsym * int) list

  let rec unifysub al = function
                        | Variable xi -> (function
                                          | Variable xi' when xi = xi' -> Some al
                                          | t' when member (vars t') xi -> None
                                          | t' -> Some ((xi, t') :: map (fun (x, a) -> (x, subst [(xi, t')] a)) al))
                        | Function (f, ts) as t -> function
                                                   | Variable xi' when member (vars t) xi' -> None
                                                   | Variable xi' -> Some ((xi', t) :: map (fun (x, a) -> (x, subst [xi', t] a)) al)
                                                   | Function (f', ts') when f = f' -> unifysublist al ts ts'
                                                   | t' -> None
  and unifysublist al ts ts' = match (ts, ts') with
                               | ([], []) -> Some al
                               | (t :: ts, []) -> None
                               | ([], t :: ts) -> None
                               | (t :: ts, t' :: ts') -> match unifysub al t t' with
                                                         | Some s -> unifysublist s (map (subst s) ts) (map (subst s) ts')
                                                         | None -> None

  let unify t t' = match unifysub [] t t' with
                   | Some s -> Some (reverse s)
                   | None -> None

  let rec crpairpart = function
                       | Variable xi -> fun ru -> []
                       | Function (f, ts) as t -> fun (l, r as ru) -> union (match unify t l with
                                                                             | Some s -> [(r, s)]
                                                                             | None -> [])
                                                                            (map (fun (ts', s') -> Function (f, ts'), s') (crpairpartlist ts ru))
  and crpairpartlist = function
                       | [] -> fun ru -> []
                       | t :: ts -> fun ru -> map (fun (t', s) -> t' :: ts, s) (crpairpart t ru) @ map (fun (ts', s) -> t :: ts', s) (crpairpartlist ts ru)

  let crpairsub r tss = map (fun (t, s) -> subst s t, subst s r) tss

  let crpair ru ru' = let (l, r as u), (l', r' as u') = uniquevar (ru, ru') in distinctswap (crpairsub r (crpairpart l u') @ crpairsub r' (crpairpart l' u))

  let symgr pre x y = match (find x pre, find y pre) with
                      | (Some i, Some j) -> i > j
                      | _ -> false

  let symeq pre x y = match (find x pre, find y pre) with
                      | _ when x = y -> true
                      | (Some i, Some j) -> i = j
                      | _ -> false

  let togr greq x y = greq x y && not (greq y x)

  let toeq greq x y = greq x y && greq y x

  let rec lexgreq greq = function
                         | [] -> (function
                                  | [] -> true
                                  | y :: ys -> false)
                         | x :: xs -> function
                                      | [] -> true
                                      | y :: ys -> if togr greq x y then true else if toeq greq x y then lexgreq greq xs ys else false

  let rec lpogreq pre t t' = match (t, t') with
                             | (t, Variable xi') -> member (vars t) xi'
                             | (Variable xi, _) -> false
                             | (Function (f, ts), Function (f', ts')) -> symeq pre f f' && lexgreq (lpogreq pre) ts ts' && all (lpogr pre t) ts' ||
                                                                         symgr pre f f' && all (lpogr pre t) ts' ||
                                                                         any (fun t'' -> lpogreq pre t'' t') ts
  and lpogr pre t t' = togr (lpogreq pre) t t'

end
