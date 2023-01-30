# Haskellでのコンストラクタの定義の方法が分からない

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)
[![クリエイティブ・コモンズ　ライセンス　BY-SA](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

[English version is here.](README.en.md)

## はじめに

この記事では，Haskellにおいて，どのように手段で利用者に値を構築させるかについて考察します．

## このファイルについて

このファイルは，[markdown-unlit](https://github.com/sol/markdown-unlit)を用いた一つの`.lhs`ファイルです．したがって，すべてのHaskellコードブロックは連結しており，あるコードブロック内で定義されている関数や有効になっているプラグマは，すべてのコードブロック内で利用可能，または有効になっています．

## ライセンス

本文は[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)の下で利用可能です．またソースコードは[WTFPL](LICENSE-WTFPL)の下で利用可能です．

## バージョン情報

記事中のコードが常に意図するように動作することを確認するために，これらはGitHub Actions上で毎日最新のGHCやCabal，Stackを用いてコンパイル，実行されています．したがって，GHCやCabal，Stackのバージョンに関しては[Actionの実行結果](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions)を，CIの詳細に関しては[.github/workflows/ci.yml](.github/workflows/ci.yml)を確認してください．

使用しているStackのレゾルバやライブラリのバージョンなどは[stack.yaml](stack.yaml)や[package.yaml](package.yaml)を確認してください．

## 値を構築する様々な方法

```haskell
module Lib
    (
    ) where
```

### 型の内部構造を公開する

#### 概要

型の内部構造を，エクスポート一覧で`Foo(..)`などとしてモジュール外に公開します．

#### コード例

```haskell
data Person = Person
    { name :: String
    , age :: Int
    }

lomias :: Person
lomias = Person {name = "ロミアス", age = 24}
```

#### 利点

- 一番コード量が少ない．
- コードがわかりやすい．
