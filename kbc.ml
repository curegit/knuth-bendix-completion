(* Knuth-Bendix 完備化 *)

module type KnuthBendixCompletionSignature = sig

  open TermRewritingSystem

  val unify : term -> term -> substitutionset option
  val crpair : rule -> rule -> equationset

end

module KnuthBendixCompletion : KnuthBendixCompletionSignature = struct

  open Utility
  open TermRewritingSystem

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

end
