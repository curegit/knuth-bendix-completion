# Knuth-Bendix Completion

クヌース・ベンディックス完備化アルゴリズムのOCaml実装

## 使い方

ディレクトリに入って`load.ml`を読み込む。
すべてのモジュールを読みこんで項書き換え系と完備化モジュールをopenするように定義されている。

```ml
#use "load.ml";;
```

### 項パーサー

`parseterm`関数を使って文字列から項を作れる。
小文字からはじまるシンボルは変数、大文字・数字・記号から始まるシンボルは関数と解釈される。

```ml
# parseterm "F(x, G(y, 0), +(H(z), 1))";;
- : TermRewritingSystem.term =
Function
 ("F",
  [Variable ("x", 0);
   Function ("G", [Variable ("y", 0); Function ("0", [])]);
   Function ("+", [Function ("H", [Variable ("z", 0)]); Function ("1", [])])])

```

### 規則の集合

### 正規形

### 簡約順序

### 等式の集合

### 完備化関数

## 群の公理の完備化

群の公理を完備化する例を示す。
