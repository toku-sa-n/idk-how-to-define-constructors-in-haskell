# Haskellでのコンストラクタの定義の方法が分からない

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)
[![クリエイティブ・コモンズ　ライセンス　BY-SA](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

[English version is here.](README.en.md)

## はじめに

この記事では，Haskellにおいて，どのように手段で利用者に値を構築させるかについて考察します．

## ライセンス

本文は[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)の下で利用可能です．またソースコードは[WTFPL](LICENSE-WTFPL)の下で利用可能です．

## バージョン情報

記事中のコードが常に意図するように動作することを確認するために，これらはGitHub Actions上で毎日最新のGHCやCabal，Stackを用いてコンパイル，実行されています．したがって，GHCやCabal，Stackのバージョンに関しては[Actionの実行結果](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions)を，CIの詳細に関しては[.github/workflows/ci.yml](.github/workflows/ci.yml)を確認してください．

使用しているStackのレゾルバやライブラリのバージョンなどは[stack.yaml](stack.yaml)や[package.yaml](package.yaml)を確認してください．

## 値を構築する様々な方法

### 型の内部構造を公開する


```haskell
module Lib
  (
  ) where
```
