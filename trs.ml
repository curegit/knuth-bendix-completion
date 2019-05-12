(* 項書換え系 *)

module type TermRewritingSystemSignature = sig

  type term = Function of (string * term list) | Variable of (string * int)
  type rule = (term * term)
  type substitution = ((string * int) * term)

  exception ParseError

  val depth : term -> int
  val vars : term -> (string * int) list
  val subst : substitution list -> term -> term
  val collate : term -> term -> substitution list option
  val rewrite : (term * term) list -> term -> term option
(*
  val lireduce : (term * term) list -> term -> term option
  val linorm : (term * term) list -> term -> term
*)
  val loreduce : (term * term) list -> term -> term option
  val lonorm : (term * term) list -> term -> term
  val poreduce : (term * term) list -> term -> term option
  val ponorm : (term * term) list -> term -> term

  val parse : string -> term
  val parsevar : string -> term
(*
  val parsefun : string -> term
*)
(*
  val printterm : term -> string
*)
end

module TermRewritingSystem : TermRewritingSystemSignature = struct

  open List
  open Char
  open String
  open TermRewritingSystemUtility

  type term = Function of (string * term list) | Variable of (string * int)
  type rule = (term * term)
  type substitution = ((string * int) * term)

  exception ParseError

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
*)
  let rec loreduce rs = function
                        | Variable xi -> rewrite rs (Variable xi)
                        | Function (f, ts) -> match rewrite rs (Function (f, ts)) with
                                              | Some t -> Some t
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

  let number chara = let c = code chara in 48 <= c && c <= 57

  let large chara = let c = code chara in 65 <= c && c <= 90

  let small chara = let c = code chara in 97 <= c && c <= 122

  let alphabet chara = number chara || large chara || small chara

  let rec parsevarsub = function
                        | "" -> ""
                        | str -> match (get str 0, parsevarsub (sub str 1 (length str - 1))) with
                                 | (' ', "") -> ""
                                 | (chara, str') -> if alphabet chara then sub str 0 1 ^ str' else raise ParseError

  let rec parsevar = function
                     | "" -> raise ParseError
                     | exp -> match get exp 0 with
                              | ' ' -> parsevar (sub exp 1 (length exp - 1))
                              | chara -> Variable (parsevarsub exp, 0)

  let rec parse = function
                      | "" -> raise ParseError
                      | exp -> match get exp 0 with
                               | ' ' -> parse (sub exp 1 (length exp - 1))
                               | head -> if number head || large head then parsefun "" false exp else if small head then parsevar exp else raise ParseError
  and parsefun sym flag = function
                              | "" -> Function (sym, [])
                              | exp -> match get exp 0 with
                                       | '(' -> Function (sym, parseargs "" [] false (sub exp 1 (length exp - 1)))
                                       | ' ' -> parsefun sym true (sub exp 1 (length exp - 1))
                                       | chara -> if flag then raise ParseError else if alphabet chara then parsefun (sym ^ sub exp 0 1) flag (sub exp 1 (length exp - 1)) else raise ParseError
  and parseargs exp terms flag = function
                                     | "" -> if flag then rev terms else raise ParseError
                                     | args -> match get args 0 with
                                               | ' ' -> parseargs (exp ^ sub args 0 1) terms flag (sub args 1 (length args - 1))
                                               | ')' -> if flag then raise ParseError else parseargs "" (parse exp :: terms) true (sub args 1 (length args - 1))
                                               | ',' -> if flag then raise ParseError else parseargs "" (parse exp :: terms) flag (sub args 1 (length args - 1))
                                               | chara -> if flag then raise ParseError else parseargs (exp ^ sub args 0 1) terms flag (sub args 1 (length args - 1))


end
