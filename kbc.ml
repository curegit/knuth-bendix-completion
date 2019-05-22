(* Knuth-Bendix 完備化 *)

module type KnuthBendixCompletionSignature = sig

  open TermRewritingSystem

  val unify : term -> term -> substitutionset option

end

module KnuthBendixCompletion : KnuthBendixCompletionSignature = struct

  open TermRewritingSystem
  open TermRewritingSystemUtility

  let rec unifysub al = function
                        | Variable xi as t -> (function
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

  let unify t t' = unifysub [] t' t

end
