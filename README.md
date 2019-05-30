# Knuth-Bendix Completion

クヌース・ベンディックス完備化アルゴリズムのOCaml実装

## 使い方

ディレクトリに入って`load.ml`を読み込む。
すべてのモジュールを読みこんで項書き換え系と完備化モジュールをopenするように定義されている。

```ml
#use "load.ml";;
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

```ml
# let precedence = [("B", 2);("S", 1);("W", 1)];;
val precedence : (string * int) list = [("B", 2); ("S", 1); ("W", 1)]
# let eqs = parseeqs ["W(x)=S(W(x))"; "W(S(x))=B(x)"];;
val eqs : TermRewritingSystem.equationset =
  [(Function ("W", [Variable ("x", 0)]),
    Function ("S", [Function ("W", [Variable ("x", 0)])]));
   (Function ("W", [Function ("S", [Variable ("x", 0)])]),
    Function ("B", [Variable ("x", 0)]))]
# let rs = kbc precedence eqs;;
val rs : TermRewritingSystem.ruleset =
  [(Function ("B", [Variable ("x", 0)]),
    Function ("W", [Function ("S", [Variable ("x", 0)])]));
   (Function ("S", [Function ("W", [Variable ("x", 0)])]),
    Function ("W", [Variable ("x", 0)]))]
# printrules rs;;
{ B(x) -> W(S(x))
  S(W(x)) -> W(x) }
- : unit = ()
```

## 群の公理の完備化

群の公理を完備化した例を示す。
