open Char
open String
open Util

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

let rec height = function
                 | Variable _ -> 1
                 | Function (_, ts) -> 1 + heightlist ts
and heightlist = function
                 | [] -> 0
                 | t :: ts -> max (height t) (heightlist ts)

let rec leaves = function
                 | Variable _ -> 1
                 | Function (_, []) -> 1
                 | Function (_, ts) -> leaveslist ts
and leaveslist = function
                 | [] -> 0
                 | t :: ts -> leaves t + leaveslist ts

let rec nodes = function
                | Variable _ -> 1
                | Function (_, ts) -> 1 + nodeslist ts
and nodeslist = function
                | [] -> 0
                | t :: ts -> nodes t + nodeslist ts

let rec vars = function
               | Variable xi -> [xi]
               | Function (_, ts) -> varslist ts
and varslist = function
               | [] -> []
               | t :: ts -> union (vars t) (varslist ts)

let rec funs = function
               | Variable _ -> []
               | Function (f, ts) -> union [f] (funslist ts)
and funslist = function
               | [] -> []
               | t :: ts -> union (funs t) (funslist ts)

let rec subst ss = function
                   | Variable xi as t -> (match find xi ss with
                                          | Some s -> s
                                          | None -> t)
                   | Function (f, ts) -> Function (f, map (subst ss) ts)

let rec collate l r = match l with
                      | Variable xi -> Some [(xi, r)]
                      | Function (f, ts) -> match r with
                                            | Variable _ -> None
                                            | Function (f', ts') -> if f = f' then collatelist ts ts' else None
and collatelist ls rs = match (ls, rs) with
                        | ([], []) -> Some []
                        | (t :: ts, t' :: ts') -> (match collate t t' with
                                                   | Some s -> (match collatelist ts ts' with
                                                                | Some s' -> append s s'
                                                                | None -> None)
                                                   | None -> None)
                        | _ -> None

let rec contain l = function
                    | Variable _ as r -> some (collate l r)
                    | Function (_, ts) as r -> some (collate l r) || containlist l ts
and containlist l = function
                    | [] -> false
                    | t :: ts -> contain l t || containlist l ts

let rec rewrite rs t = match rs with
                       | [] -> None
                       | (l, r) :: rs -> match collate l t with
                                         | Some s -> Some (subst s r)
                                         | None -> rewrite rs t

let rec lireduce rs = function
                      | Variable _ as t -> rewrite rs t
                      | Function (f, ts) as t -> match lireducelist rs ts with
                                                 | Some ts' -> Some (Function (f, ts'))
                                                 | None -> match rewrite rs t with
                                                           | Some t' -> Some t'
                                                           | None -> None
and lireducelist rs = function
                      | [] -> None
                      | t :: ts -> match lireduce rs t with
                                   | Some t' -> Some (t' :: ts)
                                   | None -> match lireducelist rs ts with
                                             | Some ts' -> Some (t :: ts')
                                             | None -> None

let rec linormtop rs = function
                       | [] -> fun t -> t
                       | (l, r) :: rs' -> fun t -> match collate l t with
                                                   | Some s -> linormsubst rs s r
                                                   | None -> linormtop rs rs' t
and linormsubst rs s = function
                       | Variable _ as t -> subst s t
                       | Function (f, ts) -> linormtop rs rs (Function (f, map (linormsubst rs s) ts))

let rec linorm rs = function
                    | Variable _ as t -> linormtop rs rs t
                    | Function (f, ts) -> linormtop rs rs (Function (f, map (linorm rs) ts))

let rec loreduce rs = function
                      | Variable _ as t -> rewrite rs t
                      | Function (f, ts) as t -> match rewrite rs t with
                                                 | Some t' -> Some t'
                                                 | None -> match loreducelist rs ts with
                                                           | Some ts' -> Some (Function (f, ts'))
                                                           | None -> None
and loreducelist rs = function
                      | [] -> None
                      | t :: ts -> match loreduce rs t with
                                   | Some t' -> Some (t' :: ts)
                                   | None -> match loreducelist rs ts with
                                             | Some ts' -> Some (t :: ts')
                                             | None -> None

let rec lonorm rs t = match loreduce rs t with
                      | Some t' -> lonorm rs t'
                      | None -> t

let rec poreduce rs = function
                      | Variable _ as t -> rewrite rs t
                      | Function (f, ts) as t -> match rewrite rs t with
                                                 | Some t' -> Some t'
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

let rec rename (xi, xi' as r) = function
                                | Variable x when x = xi -> Variable xi'
                                | Variable _ as t -> t
                                | Function (f, ts) -> Function (f, renamelist r ts)
and renamelist r = function
                   | [] -> []
                   | t :: ts -> rename r t :: renamelist r ts

let rec uniquevarstep xis (x, i) n (l, r as ru) = if member xis (x, n) then uniquevarstep xis (x, i) (n + 1) ru
                                                    else (rename ((x, i), (x, n)) l, rename ((x, i), (x, n)) r), substraction ((x, n) :: xis) [(x, i)]

let rec uniquevarsub xis ins ru = match ins with
                                  | [] -> ru
                                  | xi' :: xis' -> let (ru', xis) = uniquevarstep xis xi' 0 ru in uniquevarsub xis xis' ru'

let uniquevar (l, r as ru, (l', r' as ru')) = let uni = union (vars l) (vars r) in (ru, uniquevarsub uni (intersection uni (union (vars l') (vars r'))) ru')

let rec decvarsubstep uni (x, i) n (l, r as ru) = if member uni (x, n) then decvarsubstep uni (x, i) (n + sgn i) ru
                                                    else if abs n < abs i then (rename ((x, i), (x, n)) l, rename ((x, i), (x, n)) r), substraction ((x, n) :: uni) [(x, i)]
                                                      else ru, uni

let rec decvarsubsub uni xis ru = match xis with
                                  | [] -> ru
                                  | xi' :: xis' -> let (ru', uni') = decvarsubstep uni xi' 0 ru in decvarsubsub uni' xis' ru'

let decvarsub (l, r as ru) = let uni = union (vars l) (vars r) in decvarsubsub uni (notwhere (fun (_, i) -> i = 0) uni) ru

let sameeq (l, r) (l', r') = some (collate l l') && some (collate r r') && some (collate l' l) && some (collate r' r) || some (collate l r') && some (collate r l') && some (collate r' l) && some (collate l' r)

let rec distincteqs = function
                      | [] -> []
                      | eq :: eqs -> eq :: notwhere (sameeq eq) (distincteqs eqs)

let var x = Variable (x, 0)

let const c = Function (c, [])

let func f xs = Function (f, map var xs)

let call f ts = Function (f, ts)

let rec nest f n t = if n > 0 then Function (f, [nest f (n - 1) t]) else t

let number chara = let c = code chara in 48 <= c && c <= 57

let large chara = let c = code chara in 65 <= c && c <= 90

let small chara = let c = code chara in 97 <= c && c <= 122

let symbol chara = chara = '+' || chara = '*' || chara = '!' || chara = '?' || chara = '-' || chara = '/' || chara = '^' || chara = '$' || chara = '%' || chara = '&'

let alphabet chara = number chara || large chara || small chara || symbol chara

let rec parsevarsym = function
                      | "" -> ""
                      | str -> match (get str 0, parsevarsym (sub str 1 (length str - 1))) with
                               | (' ', "") -> ""
                               | (chara, str') when alphabet chara -> sub str 0 1 ^ str'
                               | _ -> raise ParseError

let rec parsefunsym = function
                      | "" -> ""
                      | str -> match get str 0 with
                               | '(' -> ""
                               | ' ' -> (match parsefunsym (sub str 1 (length str - 1)) with
                                         | "" -> ""
                                         | _ -> raise ParseError)
                               | chara when alphabet chara -> sub str 0 1 ^ parsefunsym (sub str 1 (length str - 1))
                               | _ -> raise ParseError

let rec parseargexpssub = function
                          | "" -> fun n -> if n = 0 then ("", []) else raise ParseError
                          | str -> fun n -> match (n, get str 0) with
                                            | (0, ' ') -> (match parseargexpssub (sub str 1 (length str - 1)) 0 with
                                                           | ("", exps) -> ("", exps)
                                                           | _ -> raise ParseError)
                                            | (0, '(') -> let (exp, exps) = parseargexpssub (sub str 1 (length str - 1)) 1 in ("", exp :: exps)
                                            | (0, _) -> raise ParseError
                                            | (1, ')') -> (match parseargexpssub (sub str 1 (length str - 1)) 0 with
                                                           | ("", []) -> ("", [])
                                                           | (_, _) -> raise ParseError)
                                            | (1, ',') -> let (exp, exps) = parseargexpssub (sub str 1 (length str - 1)) 1 in ("", exp :: exps)
                                            | (n, '(') -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) (n + 1) in (sub str 0 1 ^ str', exps)
                                            | (n, ')') -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) (n - 1) in (sub str 0 1 ^ str', exps)
                                            | (n, _) -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) n in (sub str 0 1 ^ str', exps)

let parseargexps exp = match parseargexpssub exp 0 with
                       | ("", exps) -> exps
                       | (_, _) -> raise ParseError

let rec parsevar = function
                   | "" -> raise ParseError
                   | exp -> match get exp 0 with
                            | ' ' -> parsevar (sub exp 1 (length exp - 1))
                            | _ -> Variable (parsevarsym exp, 0)

let rec parsefun = function
                   | "" -> raise ParseError
                   | exp -> match get exp 0 with
                            | ' ' -> parsefun (sub exp 1 (length exp - 1))
                            | _ -> Function (parsefunsym exp, parseargs exp)
and parseargs = function
                | "" -> []
                | exp -> match get exp 0 with
                         | ' ' -> parseargs (sub exp 1 (length exp - 1))
                         | '(' -> map parseterm (parseargexps exp)
                         | _ -> parseargs (sub exp 1 (length exp - 1))
and parseterm = function
                | "" -> raise ParseError
                | exp -> match get exp 0 with
                         | ' ' -> parseterm (sub exp 1 (length exp - 1))
                         | chara when small chara -> parsevar exp
                         | chara when large chara -> parsefun exp
                         | chara when number chara -> parsefun exp
                         | chara when symbol chara -> parsefun exp
                         | _ -> raise ParseError

let rec parsetermtuplesub deli = function
                                 | "" -> [""]
                                 | str when length str < length deli -> [str]
                                 | str when sub str 0 (length deli) = deli -> "" :: parsetermtuplesub deli (sub str (length deli) (length str - length deli))
                                 | str -> match parsetermtuplesub deli (sub str 1 (length str - 1)) with
                                          | [] -> raise ParseError
                                          | s :: ss -> (sub str 0 1 ^ s) :: ss

let parsetermtuple deli exp = match parsetermtuplesub deli exp with
                              | [l; r] -> (parseterm l, parseterm r)
                              | _ -> raise ParseError

let parserule exp = parsetermtuple "->" exp

let parseeq exp = parsetermtuple "=" exp

let parserules l = map parserule l

let parseeqs l = map parseeq l

let rec strterm = function
                  | Variable (x, i) when i = 0 -> x
                  | Variable (x, i) -> x ^ "_" ^ string_of_int i
                  | Function (f, []) -> f
                  | Function (f, ts) -> f ^ "(" ^ strtermlist ts ^ ")"
and strtermlist = function
                  | [] -> ""
                  | [t] -> strterm t
                  | t :: ts -> strterm t ^ ", " ^ strtermlist ts

let strrule (l, r) = strterm l ^ " -> " ^ strterm r

let streq (l, r) = strterm l ^ " = " ^ strterm r

let rec strtermtuplessub f = function
                             | [] -> " }"
                             | p :: ps -> "\n  " ^ f p ^ strtermtuplessub f ps

let strtermtuples f = function
                      | [] -> "{ }"
                      | p :: ps -> "{ " ^ f p ^ strtermtuplessub f ps

let strrules rs = strtermtuples strrule rs

let streqs eqs = strtermtuples streq eqs

let printterm t = print_endline (strterm t)

let printrule r = print_endline (strrule r)

let printeq eq = print_endline (streq eq)

let printrules rs = print_endline (strrules rs)

let printeqs eqs = print_endline (streqs eqs)
