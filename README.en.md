<!--
# Haskellでのコンストラクタの定義の方法が分からない
-->

# I don't know how to define constructors in Haskell

<!--
[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)
-->

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)

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

[`README.md`] is a `.lhs` file using [markdown-unlit](https://github.com/sol/markdown-unlit). Thus, all Haskell code blocks are linked, and functions defined or pragmas enabled in one code block are available or enabled in all code blocks.

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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE RecordWildCards       #-}

module Lib
    ( testInvalidPersonIsNothing
    , testPanicOnEmptyName
    , testLeftNegativeAge
    , testMkLongevity
    , testLarnneire
    ) where

import           Control.Exception
import           Test.Hspec
```
-->

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE RecordWildCards       #-}

module Lib
    ( testInvalidPersonIsNothing
    , testPanicOnEmptyName
    , testLeftNegativeAge
    , testMkLongevity
    , testLarnneire
    ) where

import           Control.Exception
import           Test.Hspec
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
    } deriving (Eq, Show)

lomias :: Person
lomias = Person {name = "ロミアス", age = 24}
```
-->

```haskell
data Person = Person
    { name :: String
    , age :: Int
    } deriving (Eq, Show)

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

<!--
### [スマートコンストラクタ](https://wiki.haskell.org/Smart_constructors)を定義する
-->

### Define [smart constructor](https://wiki.haskell.org/Smart_constructors)

<!--
#### 概要
-->

#### Overview

<!--
データ構造は公開せず，代わりに引数を基に値を生成する関数を定義します．
-->

The data structure is not exposed; instead, define a function that generates a value based on the arguments.

<!--
Haskellではそのような関数を定義する際，[`mk`という接頭辞をつけることが一般的](https://kowainik.github.io/posts/naming-conventions)のようです．
-->

In Haskell, it is common to [add the prefix `mk`](https://kowainik.github.io/posts/naming-conventions) when defining such a function.

<!--
#### コード例
-->

#### Code example

<!--
```haskell
mkPerson :: String -> Int -> Maybe Person
mkPerson name age
    | null name = Nothing
    | age < 0 = Nothing
    | otherwise = Just Person {..}

invalidPerson' :: Maybe Person
invalidPerson' = mkPerson "" (-1)

testInvalidPersonIsNothing :: Spec
testInvalidPersonIsNothing =
    describe "invalidPerson'" $
    it "`Nothing`を返す" $ invalidPerson' `shouldBe` Nothing
```
-->

```haskell
mkPerson :: String -> Int -> Maybe Person
mkPerson name age
    | null name = Nothing
    | age < 0 = Nothing
    | otherwise = Just Person {..}

invalidPerson' :: Maybe Person
invalidPerson' = mkPerson "" (-1)

testInvalidPersonIsNothing :: Spec
testInvalidPersonIsNothing =
    describe "invalidPerson'" $
    it "returns a `Nothing" $ invalidPerson' `shouldBe` Nothing
```

<!--
#### 利点
-->

#### Advantages

<!--
- データ構造の中身を公開する必要がないため，構造を変更しても他のコードを同様に変更したり，ライブラリのバージョンを上げる必要がない．（ただしコンストラクタ関数の意味やシグネチャを変更したら当然それらを行う必要はある）
- 無効なデータが生成されることを防ぐことができる．場合によっては`error`を呼び出して直ちにプログラムを終了させることができる．
-->

- Since the contents of the data structure do not need to be exposed, changing the structure does not require changing other code as well or increasing the version of the library. (However, if the semantics or signatures of the constructor functions are changed, it is of course necessary to do so.)
- It can prevent the generation of invalid data. Depending on the situation, the program can be terminated immediately by calling `error`.

<!--
```haskell
mkPerson' :: String -> Int -> Person
mkPerson' name age
    | null name = error "空の名前が渡されました．"
    | age < 0 = error "年齢が負です．"
    | otherwise = Person {..}

testPanicOnEmptyName :: Spec
testPanicOnEmptyName =
    describe "mkPerson'" $
    it "空の名前を渡すと「空の名前が渡されました．」というエラー文を表示してプログラムが終了する" $
    evaluate (mkPerson' "" 1) `shouldThrow` errorCall "空の名前が渡されました．"
```
-->

```haskell
mkPerson' :: String -> Int -> Person
mkPerson' name age
    | null name = error "An empty name is passed."
    | age < 0 = error "The age is empty."
    | otherwise = Person {..}

testPanicOnEmptyName :: Spec
testPanicOnEmptyName =
    describe "mkPerson'" $
    it "raises an error with the error message \"An empty name is passed.\"." $
    evaluate (mkPerson' "" 1) `shouldThrow` errorCall "An empty name is passed."
```

<!--
あるいは，エラー型を用いてエラーを呼び出し側に通知することもできる．
-->

Alternatively, it can notify the caller of an error by using an error type.

<!--
```haskell
data PersonError
    = EmptyName
    | NegativeAge
    deriving (Eq, Show)

mkPerson'' :: String -> Int -> Either PersonError Person
mkPerson'' name age
    | null name = Left EmptyName
    | age < 0 = Left NegativeAge
    | otherwise = Right Person {..}

testLeftNegativeAge :: Spec
testLeftNegativeAge =
    describe "mkPerson''" $
    it "負の年齢を渡すと`Left NegativeAge`を返す．" $
    mkPerson'' "Tom" (-3) `shouldBe` Left NegativeAge
```
-->

```haskell
data PersonError
    = EmptyName
    | NegativeAge
    deriving (Eq, Show)

mkPerson'' :: String -> Int -> Either PersonError Person
mkPerson'' name age
    | null name = Left EmptyName
    | age < 0 = Left NegativeAge
    | otherwise = Right Person {..}

testLeftNegativeAge :: Spec
testLeftNegativeAge =
    describe "mkPerson''" $
    it "returns a `Left NegativeAge` if a negative age is passed." $
    mkPerson'' "Tom" (-3) `shouldBe` Left NegativeAge
```

<!--
- 状況に応じて複数のコンストラクタ関数を定義できる．
-->

- Multiple constructor functions can be defined depending on the situation.

<!--
```haskell
mkLongevity :: String -> Either PersonError Person
mkLongevity name = mkPerson'' name 100

testMkLongevity :: Spec
testMkLongevity =
    describe "mkLongevity" $
    it "年齢が100歳の`Person`を生成する．" $
    mkLongevity "Tom" `shouldBe` Right Person {name = "Tom", age = 100}
```
-->


```haskell
mkLongevity :: String -> Either PersonError Person
mkLongevity name = mkPerson'' name 100

testMkLongevity :: Spec
testMkLongevity =
    describe "mkLongevity" $
    it "creates a `Person` whose age is 100 years old." $
    mkLongevity "Tom" `shouldBe` Right Person {name = "Tom", age = 100}
```

<!--
#### 欠点
-->

#### Disadvantages

<!--
- コンストラクタ関数の引数が増えるとコードが読みづらくなる．
-->

- As the number of parameters of a constructor function increases, the code becomes more difficult to read.

<!--
### Builderパターンを用いる
-->

### Using the Builder Pattern

<!--
#### 概要
-->

#### Overview

<!--
値を構築するための別の型を定義し，関数を用いて最終的な値を構築します．
-->

Define another type to construct the value and use functions to construct the final value.

<!--
#### コード例
-->

#### Code example

<!--
```haskell
data PersonBuilder = PersonBuilder
    { name :: Maybe String
    , age  :: Maybe Int
    }

mkPersonBuilder :: PersonBuilder
mkPersonBuilder = PersonBuilder {name = Nothing, age = Nothing}

-- 2023/01/30現在，PersonBuilder{..}をbuilderとして，Right builder {name = x}と
-- するとエラーが出る．
-- 詳細はhttps://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rstを確認．
setName :: String -> PersonBuilder -> Either PersonError PersonBuilder
setName x PersonBuilder {..}
    | null x = Left EmptyName
    | otherwise = Right PersonBuilder {name = Just x, ..}

setAge :: Int -> PersonBuilder -> Either PersonError PersonBuilder
setAge x PersonBuilder {..}
    | x < 0 = Left NegativeAge
    | otherwise = Right PersonBuilder {age = Just x, ..}

mkPerson''' :: PersonBuilder -> Person
mkPerson''' PersonBuilder {name = Just name, age = Just age} = Person {..}
mkPerson''' _ = error "一部の値が正しく設定されていません"

larnneire :: Either PersonError Person
larnneire =
    fmap mkPerson''' $ Right mkPersonBuilder >>= setName "ラーネイレ" >>= setAge 22

testLarnneire :: Spec
testLarnneire =
    describe "larnneire" $
    it "`Right`値を返す" $ larnneire `shouldBe` Right Person {name = "ラーネイレ", age = 22}
```
-->

```haskell
data PersonBuilder = PersonBuilder
    { name :: Maybe String
    , age  :: Maybe Int
    }

mkPersonBuilder :: PersonBuilder
mkPersonBuilder = PersonBuilder {name = Nothing, age = Nothing}

-- As of January 30, 2023, an error occurs when replacing `PersonBuilder {..}` with `builder`
-- and writing `Right builder {name = x}`.
-- See
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst
-- for the detail.
setName :: String -> PersonBuilder -> Either PersonError PersonBuilder
setName x PersonBuilder {..}
    | null x = Left EmptyName
    | otherwise = Right PersonBuilder {name = Just x, ..}

setAge :: Int -> PersonBuilder -> Either PersonError PersonBuilder
setAge x PersonBuilder {..}
    | x < 0 = Left NegativeAge
    | otherwise = Right PersonBuilder {age = Just x, ..}

mkPerson''' :: PersonBuilder -> Person
mkPerson''' PersonBuilder {name = Just name, age = Just age} = Person {..}
mkPerson''' _ = error "Some values are not set correctly."

larnneire :: Either PersonError Person
larnneire =
    fmap mkPerson''' $
    Right mkPersonBuilder >>= setName "Larnneire" >>= setAge 22

testLarnneire :: Spec
testLarnneire =
    describe "larnneire" $
    it "returns a `Right` value." $
    larnneire `shouldBe` Right Person {name = "Larnneire", age = 22}
```
