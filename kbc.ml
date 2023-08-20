open Util
open Trs

type precedence = (funsym * int) list
type lexpathorder = term -> term -> bool

exception CompletionFailed

let rec unifysub al = function
                      | Variable xi -> (function
                                        | Variable xi' when xi = xi' -> Some al
                                        | t' when member (vars t') xi -> None
                                        | t' -> Some ((xi, t') :: map (fun (x, a) -> (x, subst [(xi, t')] a)) al))
                      | Function (f, ts) as t -> function
                                                 | Variable xi' when member (vars t) xi' -> None
                                                 | Variable xi' -> Some ((xi', t) :: map (fun (x, a) -> (x, subst [xi', t] a)) al)
                                                 | Function (f', ts') when f = f' -> unifysublist al ts ts'
                                                 | _ -> None
and unifysublist al ts ts' = match (ts, ts') with
                             | ([], []) -> Some al
                             | (_, []) -> None
                             | ([], _) -> None
                             | (t :: ts, t' :: ts') -> match unifysub al t t' with
                                                       | Some s -> unifysublist s (map (subst s) ts) (map (subst s) ts')
                                                       | None -> None

let unify t t' = match unifysub [] t t' with
                 | Some s -> Some (reverse s)
                 | None -> None

let rec crpairpart = function
                     | Variable _ -> fun _ -> []
                     | Function (f, ts) as t -> fun (l, r as ru) -> union (match unify t l with
                                                                           | Some s -> [(r, s)]
                                                                           | None -> [])
                                                                      (map (fun (ts', s') -> Function (f, ts'), s') (crpairpartlist ts ru))
and crpairpartlist = function
                     | [] -> fun _ -> []
                     | t :: ts -> fun ru -> map (fun (t', s) -> t' :: ts, s) (crpairpart t ru) @ map (fun (ts', s) -> t :: ts', s) (crpairpartlist ts ru)

let crpairsub r tss = map (fun (t, s) -> subst s t, subst s r) tss

let rec distinctswap = function
                       | [] -> []
                       | (x, y as t) :: xys -> t :: notwhere (fun t' -> t = t' || (y, x) = t') (distinctswap xys)

let crpair ru ru' = let (l, r as u), (l', r' as u') = uniquevar (ru, ru') in distinctswap (crpairsub r (crpairpart l u') @ crpairsub r' (crpairpart l' u))

let symgr pre x y = match find x pre with
                    | Some i -> (match find y pre with
                                 | Some j -> i > j
                                 | None -> false)
                    | None -> false

let symeq pre x y = if x = y then true else match find x pre with
                                            | Some i -> (match find y pre with
                                                         | Some j -> i = j
                                                         | None -> false)
                                            | None -> false

let togr greq x y = greq x y && not (greq y x)

let toeq greq x y = greq x y && greq y x

let rec lexgreq greq = function
                       | [] -> (function
                                | [] -> true
                                | _ -> false)
                       | x :: xs -> function
                                    | [] -> true
                                    | y :: ys -> if togr greq x y then true else if toeq greq x y then lexgreq greq xs ys else false

let rec lpogreq pre t t' = match (t, t') with
                           | (t, Variable xi') -> member (vars t) xi'
                           | (Variable _, _) -> false
                           | (Function (f, ts), Function (f', ts')) -> symeq pre f f' && lexgreq (lpogreq pre) ts ts' && all (lpogr pre t) ts' ||
                                                                         symgr pre f f' && all (lpogr pre t) ts' ||
                                                                           any (fun t'' -> lpogreq pre t'' t') ts
and lpogr pre t t' = togr (lpogreq pre) t t'

let orientvalue (l, r) = max (nodes l) (nodes r)

let choose eq eq' = if orientvalue eq > orientvalue eq' then eq' else eq

let orient lpo (rs, eqs) = match filter (fun (l, r) -> lpo l r || lpo r l) eqs with
                           | [] -> raise CompletionFailed
                           | eq' :: eqs' -> match fold choose eq' eqs' with
                                            | (l', r' as ru) when lpo l' r' -> ((l', r'), rs, subtraction eqs [ru])
                                            | (l', r' as ru) -> ((r', l'), rs, subtraction eqs [ru])

let compose (r, rs, eqs) = (r, map (fun (l', r') -> (l', linorm (r :: rs) r')) rs, eqs)

let deduct (r, rs, eqs) = (r, rs, fold (fun eqs' eqs'' -> union eqs' eqs'') eqs (map (crpair r) (r :: rs)))

let collapse (((l, _) as ru), rs, eqs) = (ru, notwhere (fun (l', _) -> contain l l') rs, eqs)

let join (r, rs, eqs) = (r :: rs, eqs)

let simplify (rs, eqs) = (rs, map (fun (l, r) -> (linorm rs l, linorm rs r)) eqs)

let delete v (rs, eqs) = (rs, notwhere (fun (l, r) -> l = r) (if v then distincteqs eqs else eqs))

let kbcstep v lpo step = delete v (simplify (join (collapse (deduct (compose (orient lpo step))))))

let printin eqs = print_endline "================ Input =================="; printeqs eqs

let printstep n rs eqs = print_string "================ Step "; print_int n; print_endline " ================="; printeqs eqs; print_endline ""; printrules rs

let printout n rs = print_string "============== Complete "; print_int n; print_endline " ==============="; printrules rs

let rec kbcsub v = function
                   | 0 -> fun lpo ((_, eqs) as step) -> if v then printin eqs; let step' = kbcstep v lpo step in kbcsub v 1 lpo step'
                   | n -> fun lpo -> function
                                     | (rs, []) -> let rs' = map decvarsub rs in if v then printout n rs'; rs'
                                     | (rs, eqs as step) -> if v then printstep n rs eqs; let step' = kbcstep v lpo step in kbcsub v (n + 1) lpo step'

let kbcf lpo eqs = kbcsub false 0 lpo (delete false ([], eqs))

let kbc pre eqs = kbcf (lpogr pre) eqs

let kbcfv lpo eqs = kbcsub true 0 lpo (delete false ([], eqs))

let kbcv pre eqs = kbcfv (lpogr pre) eqs
