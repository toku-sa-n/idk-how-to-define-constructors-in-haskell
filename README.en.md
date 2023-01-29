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
この記事では，Haskellにおいて，どのように値を構築するかに関して考察します．
-->

This article discusses how to construct values in Haskell.

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
記事中のコードが常に意図するように動作することを確認するために，これらはGitHub Actions上で毎日最新のGHCやCabal，Stackを用いてコンパイル，実行されています．詳しくは[.github/workflows/ci.yml](.github/workflows/ci.yml)や[Actionの実行結果](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions)を確認してください．
-->

The code in this article is compiled and executed daily on GitHub Actions using the latest GHC, Cabal, and Stack to ensure that it always works as intended. Please check [.github/workflows/ci.yml](.github/workflows/ci.yml) and [results of Actions](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions) for details.

<!--
使用しているStackのレゾルバやライブラリのバージョンなどは[stack.yaml](stack.yaml)や[package.yaml](package.yaml)を確認してください．
-->

Check [stack.yaml](stack.yaml) and [package.yaml](package.yaml) for the Stack resolver and library versions used.
