# Knuth-Bendix Completion

クヌース・ベンディックス完備化アルゴリズムのOCaml実装

## OPAM, Dune と utop

### OPAM のインストール

```sh
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```

### Dune と utop のインストール

```sh
opam install dune utop
```

## 使い方

ディレクトリに入ってビルドシステム dune でコンパイルし，OCaml の対話環境 utop で読み込む。
対話環境の設定ファイル .ocamlinit により，すべてのモジュールを読みこんで項書き換え系と完備化モジュールを open するように定義されている。

```sh
dune utop
```

### 項

項は木構造を成す。ノードは変数か関数のどちらかであり、関数のみが子を持つことができる。

`parseterm`関数を使って文字列から項を作れる。
小文字からはじまるシンボルは変数、大文字・数字・記号から始まるシンボルは関数と解釈される。

使用できる記号は`+*!?-/^$%&`である。

```ml
# parseterm "F(x, G(y, 0), +(H(z), 1))";;
- : TermRewritingSystem.term =
Function
 ("F",
  [Variable ("x", 0);
   Function ("G", [Variable ("y", 0); Function ("0", [])]);
   Function ("+", [Function ("H", [Variable ("z", 0)]); Function ("1", [])])])
```

`strterm`関数で項の文字列表現を得られる。
返される文字列は必ずしも再パース可能ではないことに注意されたし（`parseterm`によって作られた項なら問題ない）。

```ml
# strterm (Function ("F", [Variable ("x", 0); Variable ("y", 0)]));;
- : string = "F(x, y)"
```

`printterm`関数は項の文字列表現を標準出力へ吐き出す。

```ml
# printterm (Function ("F", [Variable ("x", 0); Variable ("y", 0)]));;
F(x, y)
- : unit = ()
```

### 項生成ユーティリティ

- `var s : string -> term` シンボルsの変数の項を返す
- `const s : string -> term` シンボルsの無引数関数の項を返す
- `func s ss : string -> string list -> term` 仮引数の変数のシンボルのリストがssである、シンボルsの関数の項を返す
- `call s ts : string -> term list -> term` 引数の項のリストがtsである、シンボルsの関数の項を返す
- `nest s n t : string -> int -> term -> term` 項tにシンボルがsである1引数関数をn回適用した項を返す

```ml
# let f4 = call "F" [nest "S" 4 (const "0")];;
val f4 : TermRewritingSystem.term =
  Function
   ("F",
    [Function
      ("S",
       [Function
         ("S", [Function ("S", [Function ("S", [Function ("0", [])])])])])])
# printterm f4;;
F(S(S(S(S(0)))))
- : unit = ()
```

### 規則

書き換え規則は項の組で表す。

`parserule`関数によって文字列から規則を得る。
文字列は`項 -> 項`の形とする。

```ml
# parserule "F(x, y) -> G(y, x)";;
- : TermRewritingSystem.rule =
(Function ("F", [Variable ("x", 0); Variable ("y", 0)]),
 Function ("G", [Variable ("y", 0); Variable ("x", 0)]))
```

`strrule`関数で書き換え規則の文字列表現を得る。
`printrule`関数は書き換え規則を標準出力する。

### 規則の集合

書き換え規則の集合は書き換え規則のリストで表す。
`parserules`関数は規則の文字列のリストから規則の集合を返す。

```ml
# let rs = parserules ["A(0, y) -> y"; "A(S(x), y) -> S(A(x, y))"];;
val rs : TermRewritingSystem.ruleset =
  [(Function ("A", [Function ("0", []); Variable ("y", 0)]),
    Variable ("y", 0));
   (Function ("A", [Function ("S", [Variable ("x", 0)]); Variable ("y", 0)]),
    Function ("S", [Function ("A", [Variable ("x", 0); Variable ("y", 0)])]))]
```

`strrules`関数は書き換え規則の集合の文字列表現を得る。
`printrules`関数は書き換え規則の集合を標準出力する。

```ml
# strrules rs;;
- : string = "{ A(0, y) -> y\n  A(S(x), y) -> S(A(x, y)) }"
# printrules rs;;
{ A(0, y) -> y
  A(S(x), y) -> S(A(x, y)) }
- : unit = ()
```

### 正規形

`linorm`、`lonorm`及び`ponorm`は書き換え規則の集合と項を与えると正規形を求める関数である。
それぞれ最左最内戦略、最左最外戦略、並列最外戦略に対応する。

#### フィボナッチ数

以下は最左最内戦略でフィボナッチの7を評価した例である。

```ml
# let rs = parserules ["Add(0,y)->y"; "Add(S(x),y)->S(Add(x, y))"; "Fib(0)->0";
"Fib(S(0))->S(0)"; "Fib(S(S(x)))->Add(Fib(x),Fib(S(x)))"];;
val rs : TermRewritingSystem.ruleset =
  [(Function ("Add", [Function ("0", []); Variable ("y", 0)]),
    Variable ("y", 0));
   (Function
     ("Add", [Function ("S", [Variable ("x", 0)]); Variable ("y", 0)]),
    Function
     ("S", [Function ("Add", [Variable ("x", 0); Variable ("y", 0)])]));
   (Function ("Fib", [Function ("0", [])]), Function ("0", []));
   (Function ("Fib", [Function ("S", [Function ("0", [])])]),
    Function ("S", [Function ("0", [])]));
   (Function ("Fib", [Function ("S", [Function ("S", [Variable ("x", 0)])])]),
    Function
     ("Add",
      [Function ("Fib", [Variable ("x", 0)]);
       Function ("Fib", [Function ("S", [Variable ("x", 0)])])]))]
# printrules rs;;
{ Add(0, y) -> y
  Add(S(x), y) -> S(Add(x, y))
  Fib(0) -> 0
  Fib(S(0)) -> S(0)
  Fib(S(S(x))) -> Add(Fib(x), Fib(S(x))) }
- : unit = ()
# let t = call "Fib" [nest "S" 7 (const "0")];;
val t : TermRewritingSystem.term =
  Function
   ("Fib",
    [Function
      ("S",
       [Function
         ("S",
          [Function
            ("S",
             [Function
               ("S",
                [Function
                  ("S",
                   [Function ("S", [Function ("S", [Function ("0", [])])])])])])])])])
# let nf = linorm rs t;;
val nf : TermRewritingSystem.term =
  Function
   ("S",
    [Function
      ("S",
       [Function
         ("S",
          [Function
            ("S",
             [Function
               ("S",
                [Function
                  ("S",
                   [Function
                     ("S",
                      [Function
                        ("S",
                         [Function
                           ("S",
                            [Function
                              ("S",
                               [Function
                                 ("S",
                                  [Function
                                    ("S",
                                     [Function ("S", [Function ("0", [])])])])])])])])])])])])])])
# printterm nf;;
S(S(S(S(S(S(S(S(S(S(S(S(S(0)))))))))))))
- : unit = ()
```

### 等式

等式は規則と同じく項の組で表す。

`parseeq`関数によって文字列から等式を得る。
文字列は`項 = 項`の形とする。

```ml
# parseeq "F(G(x)) = G(F(x))";;
- : TermRewritingSystem.equation =
(Function ("F", [Function ("G", [Variable ("x", 0)])]),
 Function ("G", [Function ("F", [Variable ("x", 0)])]))
```

`streq`関数で等式の文字列表現を得る。
`printeq`関数は等式を標準出力する。

### 等式の集合

等式の集合は等式のリストで表す。
`parseeqs`関数は等式の文字列のリストから等式の集合を返す。

```ml
# let eqs = parseeqs ["A = B"; "B = C"];;
val eqs : TermRewritingSystem.equationset =
  [(Function ("A", []), Function ("B", []));
   (Function ("B", []), Function ("C", []))]
```
`streqs`関数は等式の集合の文字列表現を得る。
`printeqs`関数は等式の集合を標準出力する。

```ml
# streqs eqs;;
- : string = "{ A = B\n  B = C }"
# printeqs eqs;;
{ A = B
  B = C }
- : unit = ()
```

### 簡約化順序

完備化のために停止性を保証する簡約化順序を与える必要がある。

関数のシンボルと整数値の組のリスト（優先順位を表すリスト）によってシンボル上の大小関係を定義する。

```ml
# let prece = [("F", 2);("G", 3);("H", 1)];;
val prece : (string * int) list = [("F", 2); ("G", 3); ("H", 1)]
```

### 完備化

`kbc`と`kbcf`が完備化を行う関数である。
`kbc`は優先順位を表すリストと等式集合を引数にとり、辞書式経路順序によって完備化を行う。
`kbcf`は簡約化順序を示す順序関数と等式集合を引数にとって完備化を行う。

`kbcv`と`kbcfv`はそれぞれの関数の途中経過を標準出力するバージョンである（重複等式の削除など一部の冗長な処理も追加される）。

完備化に失敗すると例外が投げられる。
アルゴリズムが停止しない可能性もある。

#### グラス置き換えパズル

以下はグラス置き換えパズル（酒・ウィスキー・ビール）の問題を完備化した例である。

2つのグラス列が与えられ、等式に従ったグラス交換でもう一方と同じ列を作れるかどうかという問題である。

```ml
# let precedence = [("B", 3);("S", 2);("W", 1)];;
val precedence : (string * int) list = [("B", 3); ("S", 2); ("W", 1)]
# let eqs = parseeqs ["W(x)=S(W(x))"; "W(S(x))=B(x)"; "B(x)=B(B(x))"];;
val eqs : TermRewritingSystem.equationset =
  [(Function ("W", [Variable ("x", 0)]),
    Function ("S", [Function ("W", [Variable ("x", 0)])]));
   (Function ("W", [Function ("S", [Variable ("x", 0)])]),
    Function ("B", [Variable ("x", 0)]));
   (Function ("B", [Variable ("x", 0)]),
    Function ("B", [Function ("B", [Variable ("x", 0)])]))]
# let rs = kbc precedence eqs;;
val rs : TermRewritingSystem.ruleset =
  [(Function ("W", [Function ("W", [Function ("W", [Variable ("x", 0)])])]),
    Function ("W", [Function ("W", [Variable ("x", 0)])]));
   (Function ("W", [Function ("W", [Function ("S", [Variable ("x", 0)])])]),
    Function ("W", [Function ("S", [Variable ("x", 0)])]));
   (Function ("B", [Variable ("x", 0)]),
    Function ("W", [Function ("S", [Variable ("x", 0)])]));
   (Function ("S", [Function ("W", [Variable ("x", 0)])]),
    Function ("W", [Variable ("x", 0)]))]
# printrules rs;;
{ W(W(W(x))) -> W(W(x))
  W(W(S(x))) -> W(S(x))
  B(x) -> W(S(x))
  S(W(x)) -> W(x) }
- : unit = ()
```

得られた書き換え規則を使って、例えば`SBW`と`SSB`の列を評価すると異なる正規形が求まる。
正規形が異なるのでグラス交換で`SBW`と`SSB`を行き来することができないと証明される。

```ml
# let t1 = parseterm "S(B(W(END)))";;
val t1 : TermRewritingSystem.term =
  Function ("S", [Function ("B", [Function ("W", [Function ("END", [])])])])
# let t2 = parseterm "S(S(B(END)))";;
val t2 : TermRewritingSystem.term =
  Function ("S", [Function ("S", [Function ("B", [Function ("END", [])])])])
# linorm rs t1;;
- : TermRewritingSystem.term =
Function ("W", [Function ("W", [Function ("END", [])])])
# linorm rs t2;;
- : TermRewritingSystem.term =
Function ("W", [Function ("S", [Function ("END", [])])])
```

## 群の公理の完備化

群の公理を完備化した例を示す。
`kbcv`関数を使って途中経過もすべて出力している。

```ml
# let precedence = [("I", 3); ("G", 2); ("E", 1)];;
val precedence : (string * int) list = [("I", 3); ("G", 2); ("E", 1)]
# let eqs = parseeqs ["G(E,x)=x"; "G(I(x),x)=E"; "G(G(x,y),z)=G(x,G(y,z))"];;
val eqs : TermRewritingSystem.equationset =
  [(Function ("G", [Function ("E", []); Variable ("x", 0)]),
    Variable ("x", 0));
   (Function ("G", [Function ("I", [Variable ("x", 0)]); Variable ("x", 0)]),
    Function ("E", []));
   (Function
     ("G",
      [Function ("G", [Variable ("x", 0); Variable ("y", 0)]);
       Variable ("z", 0)]),
    Function
     ("G",
      [Variable ("x", 0);
       Function ("G", [Variable ("y", 0); Variable ("z", 0)])]))]
# kbcv precedence eqs;;
================ Input ==================
{ G(E, x) = x
  G(I(x), x) = E
  G(G(x, y), z) = G(x, G(y, z)) }
================ Step 1 =================
{ G(I(x), x) = E
  G(G(x, y), z) = G(x, G(y, z)) }

{ G(E, x) -> x }
================ Step 2 =================
{ G(G(x, y), z) = G(x, G(y, z)) }

{ G(I(x), x) -> E
  G(E, x) -> x }
================ Step 3 =================
{ z = G(I(x_1), G(x_1, z)) }

{ G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 4 =================
{ G(I(I(x)), z_1) = G(x, z_1)
  G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(I(I(x)), E) = x
  G(I(E), x) = x }

{ G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 5 =================
{ G(I(I(x)), z_1) = G(x, z_1)
  G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(I(I(x)), E) = x
  G(I(I(E)), x) = x }

{ G(I(E), x) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 6 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(x, E) = x
  z = G(x, G(I(x), z))
  E = G(x, I(x)) }

{ G(I(I(x)), z_1) -> G(x, z_1)
  G(I(E), x) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 7 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  z = G(x, G(I(x), z))
  E = G(x, I(x))
  x_1 = I(I(x_1))
  E = I(E) }

{ G(x, E) -> x
  G(I(I(x)), z_1) -> G(x, z_1)
  G(I(E), x) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 8 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  z = G(x, G(I(x), z))
  E = G(x, I(x))
  x_1 = I(I(x_1)) }

{ I(E) -> E
  G(x, E) -> x
  G(I(I(x)), z_1) -> G(x, z_1)
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 9 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  z = G(x, G(I(x), z))
  E = G(x, I(x)) }

{ I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 10 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  z = G(x, G(I(x), z))
  G(x_1, G(y, I(G(x_1, y)))) = E }

{ G(x, I(x)) -> E
  I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 11 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(x_1, G(y, I(G(x_1, y)))) = E
  G(x_1, G(y, G(I(G(x_1, y)), z))) = z }

{ G(x, G(I(x), z)) -> z
  G(x, I(x)) -> E
  I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 12 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(x_1, G(y, G(I(G(x_1, y)), z))) = z
  x = G(y, I(G(I(x), y)))
  I(x_1) = G(y, I(G(x_1, y)))
  G(x, G(y_1, G(y, I(G(x, G(y_1, y)))))) = E }

{ G(x_1, G(y, I(G(x_1, y)))) -> E
  G(x, G(I(x), z)) -> z
  G(x, I(x)) -> E
  I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 13 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(x_1, G(y, G(I(G(x_1, y)), z))) = z
  G(x, G(y_1, G(y, I(G(x, G(y_1, y)))))) = E
  G(I(G(x, y_1)), x) = I(y_1)
  G(y, G(I(G(x_1, y)), x_1)) = E
  G(x, I(x_1)) = I(G(x_1, I(x)))
  G(I(y), I(x_1)) = I(G(x_1, y))
  G(x, G(y_1, I(G(x_1, G(x, y_1))))) = I(x_1)
  G(z, I(G(x, G(y_1, z)))) = I(G(x, y_1))
  G(I(x_1), z) = G(y, G(I(G(x_1, y)), z)) }

{ G(y, I(G(x_1, y))) -> I(x_1)
  G(x, G(I(x), z)) -> z
  G(x, I(x)) -> E
  I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
================ Step 14 =================
{ G(I(G(x, y)), G(x, G(y, z_1))) = z_1
  G(x_1, G(y, G(I(G(x_1, y)), z))) = z
  G(x, G(y_1, G(y, I(G(x, G(y_1, y)))))) = E
  G(I(G(x, y_1)), x) = I(y_1)
  G(y, G(I(G(x_1, y)), x_1)) = E
  G(I(y), I(x_1)) = I(G(x_1, y))
  G(x, G(y_1, I(G(x_1, G(x, y_1))))) = I(x_1)
  G(z, I(G(x, G(y_1, z)))) = I(G(x, y_1))
  G(I(x_1), z) = G(y, G(I(G(x_1, y)), z))
  I(G(x_1, G(x_1, I(x_2)))) = G(x_2, G(I(x_1), I(x_1)))
  I(G(x_2, G(y, I(x)))) = G(x, I(G(x_2, y))) }

{ I(G(x_1, I(x))) -> G(x, I(x_1))
  G(y, I(G(x_1, y))) -> I(x_1)
  G(x, G(I(x), z)) -> z
  G(x, I(x)) -> E
  I(I(x_1)) -> x_1
  I(E) -> E
  G(x, E) -> x
  G(I(x_1), G(x_1, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
============== Complete 15 ===============
{ I(G(x, y)) -> G(I(y), I(x))
  G(x, G(I(x), z)) -> z
  G(x, I(x)) -> E
  I(I(x)) -> x
  I(E) -> E
  G(x, E) -> x
  G(I(x), G(x, z)) -> z
  G(G(x, y), z) -> G(x, G(y, z))
  G(I(x), x) -> E
  G(E, x) -> x }
- : TermRewritingSystem.ruleset =
[(Function ("I", [Function ("G", [Variable ("x", 0); Variable ("y", 0)])]),
  Function
   ("G",
    [Function ("I", [Variable ("y", 0)]);
     Function ("I", [Variable ("x", 0)])]));
 (Function
   ("G",
    [Variable ("x", 0);
     Function ("G", [Function ("I", [Variable ("x", 0)]); Variable ("z", 0)])]),
  Variable ("z", 0));
 (Function ("G", [Variable ("x", 0); Function ("I", [Variable ("x", 0)])]),
  Function ("E", []));
 (Function ("I", [Function ("I", [Variable ("x", 0)])]), Variable ("x", 0));
 (Function ("I", [Function ("E", [])]), Function ("E", []));
 (Function ("G", [Variable ("x", 0); Function ("E", [])]), Variable ("x", 0));
 (Function
   ("G",
    [Function ("I", [Variable ("x", 0)]);
     Function ("G", [Variable ("x", 0); Variable ("z", 0)])]),
  Variable ("z", 0));
 (Function
   ("G",
    [Function ("G", [Variable ("x", 0); Variable ("y", 0)]);
     Variable ("z", 0)]),
  Function
   ("G",
    [Variable ("x", 0);
     Function ("G", [Variable ("y", 0); Variable ("z", 0)])]));
 (Function ("G", [Function ("I", [Variable ("x", 0)]); Variable ("x", 0)]),
  Function ("E", []));
 (Function ("G", [Function ("E", []); Variable ("x", 0)]), Variable ("x", 0))]
```
