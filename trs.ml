(* 項書換え系 *)

module type TermRewritingSystemSignature = sig

  type funsym = string
  type varsym = string * int
  type term = Variable of varsym | Function of (funsym * term list)
  type rule = term * term
  type equation = term * term
  type substitution = varsym * term

  exception ParseError

  val depth : term -> int
  val vars : term -> varsym list
  val subst : substitution list -> term -> term
  val collate : term -> term -> substitution list option
  val rewrite : rule list -> term -> term option

  val lireduce : rule list -> term -> term option
  val linorm : rule list -> term -> term
  val loreduce : rule list -> term -> term option
  val lonorm : rule list -> term -> term
  val poreduce : rule list -> term -> term option
  val ponorm : rule list -> term -> term

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
(*
  val printterm : term -> string
*)
end

module TermRewritingSystem : TermRewritingSystemSignature = struct

  open Char
  open String
  open TermRewritingSystemUtility

  type funsym = string
  type varsym = string * int
  type term = Variable of varsym | Function of (funsym * term list)
  type rule = term * term
  type equation = term * term
  type substitution = varsym * term

  exception ParseError

  let rec depth = function
                  | Variable xi -> 1
                  | Function (f, ts) -> 1 + depthlist ts
  and depthlist = function
                  | [] -> 0
                  | t :: ts -> max (depth t) (depthlist ts)

  let rec vars = function
                 | Variable xi -> [xi]
                 | Function (f, ts) -> varslist ts
  and varslist = function
                 | [] -> []
                 | t :: ts -> union (vars t) (varslist ts)

  let rec subst ss = function
                     | Variable xi as t -> (match find xi ss with
                                            | Some s -> s
                                            | None -> t)
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
                                                     | _ -> None)
                          | _ -> None

  let rec rewrite rs t = match rs with
                         | [] -> None
                         | (l, r) :: rs -> match collate l t with
                                           | Some s -> Some (subst s r)
                                           | None -> rewrite rs t

  let rec lireduce rs = function
                        | Variable xi as t -> rewrite rs t
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
                         | Variable xi as t -> subst s t
                         | Function (f, ts) -> linormtop rs rs (Function (f, map (linormsubst rs s) ts))

  let rec linorm rs = function
                      | Variable xi as t -> linormtop rs rs t
                      | Function (f, ts) -> linormtop rs rs (Function (f, map (linorm rs) ts))

  let rec loreduce rs = function
                        | Variable xi as t -> rewrite rs t
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
                        | Variable xi as t -> rewrite rs t
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

  let var x = Variable (x, 0)

  let const c = Function (c, [])

  let func f xs = Function (f, map var xs)

  let call f ts = Function (f, ts)

  let rec nest f n t = if n > 0 then Function (f, [nest f (n - 1) t]) else t

  let number chara = let c = code chara in 48 <= c && c <= 57

  let large chara = let c = code chara in 65 <= c && c <= 90

  let small chara = let c = code chara in 97 <= c && c <= 122

  let alphabet chara = number chara || large chara || small chara

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
                                              | (0, chara) -> raise ParseError
                                              | (1, ')') -> (match parseargexpssub (sub str 1 (length str - 1)) 0 with
                                                             | ("", []) -> ("", [])
                                                             | (str', exps) -> raise ParseError)
                                              | (1, ',') -> let (exp, exps) = parseargexpssub (sub str 1 (length str - 1)) 1 in ("", exp :: exps)
                                              | (n, '(') -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) (n + 1) in (sub str 0 1 ^ str', exps)
                                              | (n, ')') -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) (n - 1) in (sub str 0 1 ^ str', exps)
                                              | (n, chara) -> let (str', exps) = parseargexpssub (sub str 1 (length str - 1)) n in (sub str 0 1 ^ str', exps)

  let parseargexps exp = match parseargexpssub exp 0 with
                         | ("", exps) -> exps
                         | (str, exps) -> raise ParseError

  let rec parsevar = function
                     | "" -> raise ParseError
                     | exp -> match get exp 0 with
                              | ' ' -> parsevar (sub exp 1 (length exp - 1))
                              | chara -> Variable (parsevarsym exp, 0)

  let rec parsefun = function
                     | "" -> raise ParseError
                     | exp -> match get exp 0 with
                              | ' ' -> parsefun (sub exp 1 (length exp - 1))
                              | chara -> Function (parsefunsym exp, parseargs exp)
  and parseargs = function
                  | "" -> []
                  | exp -> match get exp 0 with
                           | ' ' -> parseargs (sub exp 1 (length exp - 1))
                           | '(' -> map parseterm (parseargexps exp)
                           | chara -> parseargs (sub exp 1 (length exp - 1))
  and parseterm = function
                  | "" -> raise ParseError
                  | exp -> match get exp 0 with
                           | ' ' -> parseterm (sub exp 1 (length exp - 1))
                           | chara when small chara -> parsevar exp
                           | chara when large chara -> parsefun exp
                           | chara when number chara -> parsefun exp
                           | chara -> raise ParseError

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

end
