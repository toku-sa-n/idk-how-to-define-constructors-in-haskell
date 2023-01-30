<!--
# Haskellでのコンストラクタの定義の方法が分からない
-->

# I don't know how to define constructors in Haskell

<!--
[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)
[![クリエイティブ・コモンズ　ライセンス　BY-SA](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
-->

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)
[![Creative Commons License BY-SA](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

<!--
[English version is here.](README.en.md)
-->

[日本語版はこちら．](src/Lib.lhs)

<!--
## はじめに
-->

## Introduction

<!--
この記事では，Haskellにおいて，どのように手段で利用者に値を構築させるかについて考察します．
-->

This article discusses how to let users construct values in Haskell.

<!--
## このファイルについて
-->

## About this file

<!--
このファイルは，[markdown-unlit](https://github.com/sol/markdown-unlit)を用いた一つの`.lhs`ファイルです．したがって，すべてのHaskellコードブロックは連結しており，あるコードブロック内で定義されている関数や有効になっているプラグマは，すべてのコードブロック内で利用可能，または有効になっています．
-->

This is a single `.lhs` file using [markdown-unlit](https://github.com/sol/markdown-unlit). Thus, all Haskell code blocks are linked, and functions defined or pragmas enabled in one code block are available or enabled in all code blocks.

<!--
## ライセンス
-->

## License

<!--
本文は[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)の下で利用可能です．またソースコードは[WTFPL](LICENSE-WTFPL)の下で利用可能です．
-->

The text is available under [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). Source code is available under [WTFPL](LICENSE-WTFPL).

<!--
## バージョン情報
-->

## Version information

<!--
記事中のコードが常に意図するように動作することを確認するために，これらはGitHub Actions上で毎日最新のGHCやCabal，Stackを用いてコンパイル，実行されています．したがって，GHCやCabal，Stackのバージョンに関しては[Actionの実行結果](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions)を，CIの詳細に関しては[.github/workflows/ci.yml](.github/workflows/ci.yml)を確認してください．
-->

The code in the article is compiled and executed daily on GitHub Actions using the latest GHC, Cabal, and Stack to ensure that it always works as intended. Therefore, please refer to [the results of Actions](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions) for the versions of GHC, Cabal, and Stack, and to [.github/workflows/ci.yml](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions) for the details of the CI.

<!--
使用しているStackのレゾルバやライブラリのバージョンなどは[stack.yaml](stack.yaml)や[package.yaml](package.yaml)を確認してください．
-->

Check [stack.yaml](stack.yaml) and [package.yaml](package.yaml) for the Stack resolver and library versions used.

<!--
## 値を構築する様々な方法
-->

## Different ways to construct values

<!--
```haskell
module Lib
    (
    ) where
```
-->

```haskell
module Lib
    (
    ) where
```

<!--
### 型の内部構造を公開する
-->

### Expose the internal structure of a type

<!--
#### 概要
-->

#### Overview

<!--
型の内部構造を，エクスポート一覧で`Foo(..)`などとしてモジュール外に公開します．
-->

Expose the internal structure of the type in the export list as `Foo(...) ` etc.

<!--
#### コード例
-->

#### Code example

<!--
```haskell
data Person = Person
    { name :: String
    , age :: Int
    }

lomias :: Person
lomias = Person {name = "ロミアス", age = 24}
```
-->

```haskell
data Person = Person
    { name :: String
    , age :: Int
    }

lomias :: Person
lomias = Person {name = "Lomias", age = 24}
```

<!--
#### 利点
-->

#### Advantages

<!--
- 一番コード量が少ない．
- コードがわかりやすい．
-->

- The least amount of code.
- The code is easy to understand.

<!--
#### 欠点
-->

#### Disadvantages


<!--
- データ構造を変更すると，そのデータ構造を使用しているすべてのコードを変更する必要がある．
- データ構造をライブラリとして公開している場合，データ構造の変更は破壊的変更となり，バージョンを上げる必要がある．
- 妥当ではない値も生成できてしまう．
-->

- Changing a data structure requires changing all code that uses that structure.
- If a data structure is published as a library, changing the structure is a breaking change and requires a version upgrade.
- It is also possible to generate invalid values.

<!--
```haskell
invalidPerson :: Person
invalidPerson = Person {name = "", age = -1}
```
-->

```haskell
invalidPerson :: Person
invalidPerson = Person {name = "", age = -1}
```

<!--
- セレクタ関数をエクスポートすることで，名前空間を圧迫する．ただしこの問題は，[`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html)や[`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html)，[`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html)を用いると，そこまで問題ではなくなる．Haskell Day 2021のfumieval氏の発表「[Haskell は別言語になりました――RecordDotSyntax と NoFieldSelectors](https://youtu.be/haZl-q6mfyk?t=2581)」も参考．
-->

- Exporting selector function overwhelms the namespace. However, it doesn't really matter because of [`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html), [`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html), and [`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html). Also refer to the presentation "Haskell is now a different language――RecordDotSyntax and NoFieldSelectors](https://youtu.be/haZl-q6mfyk?t=2581)". (In Japanese).
