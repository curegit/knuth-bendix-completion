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
文字列のリストを`parserule`で写像すると良い。

```ml
# let rs = List.map parserule ["A(0, y) -> y"; "A(S(x), y) -> S(A(x, y))"];;
val rs : TermRewritingSystem.rule list =
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

`streq`関数で書き換え規則の文字列表現を得る。
`printeq`関数は書き換え規則を標準出力する。

### 等式の集合

等式の集合は等式のリストで表す。
文字列のリストを`parseeq`で写像すると良い。

```ml
# List.map parseeq ["A = B"; "B = C"];;
- : TermRewritingSystem.equation list =
[(Function ("A", []), Function ("B", []));
 (Function ("B", []), Function ("C", []))]
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

`kbcv`と`kbcfv`はそれぞれの関数の途中経過を標準出力するバージョンである。

```ml
# let prece = [("B", 2);("S", 1);("W", 1)];;
val prece : (string * int) list = [("B", 2); ("S", 1); ("W", 1)]
# let eqs = List.map parseeq ["W(x)=S(W(x))"; "W(S(x))=B(x)"];;
val eqs : TermRewritingSystem.equation list =
  [(Function ("W", [Variable ("x", 0)]),
    Function ("S", [Function ("W", [Variable ("x", 0)])]));
   (Function ("W", [Function ("S", [Variable ("x", 0)])]),
    Function ("B", [Variable ("x", 0)]))]
# let rs = kbc prece eqs;;
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