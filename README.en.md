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
この記事では，Haskellにおいて，どのような手段で型の利用者にその型の値を構築させるかについて考察します．
-->

This article discusses how to let users of a type construct the value of that type in Haskell.

<!--
## リポジトリ内のファイルについて
-->

## About files in this repository

<!--
このリポジトリは，単一のStackライブラリとなっています．リポジトリのルートディレクトリで`stack test`を実行することで，このライブラリをテストすることができます．
-->

This repository is a single Stack library. You can test this library by running `stack test` in the repository root directory.

<!--
[`src/Lib.lhs`](/src/Lib.lhs)では[markdown-unlit](https://github.com/sol/markdown-unlit)を用いており，[`README.md`](/README.md)はそのファイルへのシンボリックリンクです．ファイル内のすべてのHaskellコードブロックは連結しており，それらをすべて順番に結合したものが最終的なコードとなります．
-->

I use [markdown-unlit](https://github.com/sol/markdown-unlit) in [`src/Lib.lhs`](/src/Lib.lhs), and [`README.md`](/README.md) is a symbolic link to it. All Haskell code blocks in the file are linked together, and the final code is obtained by concatenating them all in sequence.

<!--
なお，英語版のREADMEである[`README.en.md`](/README.en.md)は`.lhs`ファイルではありませんが，[`README.md`](/README.md)同様，すべてのコードブロックは連結しているものと見做してください．
-->

Note that [`README.en.md`](/README.en.md), the English version of README, is not a `.lhs` file. However, please assume that all code blocks are linked, as in [`README.md`](/README.md).

<!--
## ライセンス
-->

## License

<!--
本文は[CC BY-SA 4.0](/LICENSE-CC-BY-SA)の下で利用可能です．またソースコードは[WTFPL](LICENSE-WTFPL)の下で利用可能です．
-->

The text is available under [CC BY-SA 4.0](/LICENSE-CC-BY-SA). Source code is available under [WTFPL](LICENSE-WTFPL).

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
## 評価
-->

## Evaluation

<!--
これから紹介する方法を，以下の観点で評価していきます．
-->

I will evaluate the methods I will present in the following perspectives.

<!--
### 値の構築手段を提供するためのコードの量
-->

### The amount of code to provide a means of constructing a value

<!--
型自体の定義以外に必要とするコードの量です．基本的に短いほうが良いです．
-->

The amount of code required other than the type definition itself. Basically, the shorter it is, the better.

<!--
### 値を構築するためのコードの量
-->

### The amount of code to construct a value

<!--
用意された手段で値を構築しようとする際に必要とするコードの量です．こちらも基本的に短いほうが良いです．
-->

The amount of code required to construct a value using the provided means. Again, basically, the shorter it is, the better.

<!--
### 型の内部構造を隠蔽できるかどうか
-->

### Whether the internal structure of the type can be hidden

<!--
型の内部構造は基本的に隠蔽するべきです．公開した場合，その構造を変更するとそれに依存しているすべてのコードを変更する必要があります．また，生成された値があとから変更されてしまい，不正な値となってしまう恐れもあります．更に，型を内部構造も含めてライブラリとして公開している場合，構造を変更する際はライブラリのバージョンを上げる必要があります．
-->

The internal structure of a type should be basically hidden. If it is exposed, any change to the structure will require a change to all code that depends on it. Also, the generated values may later be modified, resulting in invalid values. Furthermore, if the type, including its internal structure, is available as a library, the library version must be upgraded when the structure is changed.

<!--
### 不正な値の生成を防ぐことができるかどうか
-->

### Whether the generation of illegal values can be prevented

<!--
空の名前や負の年齢などの値域から外れている値や，同じ要素を複数個含む集合など，値が持つべき前提から逸脱しているような値はそもそも生成されるべきではありません．またそのような値を生成しようとした際は，エラーを返すなどとして適切に対処するべきです．
-->

Values that deviate from the assumptions that values should have, such as empty names, negative ages, and sets that contain multiple elements of the same value, should not be generated in the first place. If such a value is attempted to be generated, it should be handled appropriately such as by returning an error message.

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
    ( Person(..)
    , Foo(..)
    , testFooDef
    , testInvalidPersonIsNothing
    , testPanicOnEmptyName
    , testLeftNegativeAge
    , testMkLongevity
    , testLarnneire
    , testLoyter
    ) where

import           Control.Exception
import           Data.Default
import           Test.Hspec
```
-->

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE RecordWildCards       #-}

module Lib
    ( Person(..)
    , Foo(..)
    , testFooDef
    , testInvalidPersonIsNothing
    , testPanicOnEmptyName
    , testLeftNegativeAge
    , testMkLongevity
    , testLarnneire
    , testLoyter
    ) where

import           Control.Exception
import           Data.Default
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
エクスポート一覧の例は上部にあります．
-->

An example of the export list is given above.

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
#### 評価
-->

#### Evaluation

<!--
##### 値の構築手段を提供するためのコードの量
-->

##### The amount of code to provide a means of constructing a value

<!--
単にエクスポート一覧で`Person(..)`などと書けばよいため，提供側のコード量は非常に少ないです．
-->

The amount of code on the provider side is very small, since you can simply write `Person(...)` in the export list.

<!--
##### 値を構築するためのコードの量
-->

##### The amount of code to construct a value

<!--
これは型によります．値コンストラクタが持つ引数やレコードのフィールド数が大きくなるとそれだけコードは長くなります．
-->

It depends on the type. The larger the number of arguments or record fields that a value constructor has, the longer the code will be.

<!--
ただし，予めデフォルト値を提供し，必要ならば利用者にその一部を変更してもらうという方法にすると，コードを削減できる場合があります．一例として，[`data-default`](https://hackage.haskell.org/package/data-default)を使用した方法を紹介します．
-->

However, it may be possible to reduce the amount of code by providing a default value in advance and allowing users to change parts of the value if necessary. As an example, here is a method using [`data-default`](https://hackage.haskell.org/package/data-default).

<!--
```haskell
data Foo = Foo
    { bar :: Int
    , baz :: Int
    } deriving (Eq, Show)

instance Default Foo where
    def = Foo {bar = 0, baz = 0}

foo :: Foo
foo = def {bar = 1}

testFooDef :: Spec
testFooDef =
    describe "foo" $
    it "Foo {bar = 1, baz = 0}" $ foo `shouldBe` Foo {bar = 1, baz = 0}
```
-->

```haskell
data Foo = Foo
    { bar :: Int
    , baz :: Int
    } deriving (Eq, Show)

instance Default Foo where
    def = Foo {bar = 0, baz = 0}

foo :: Foo
foo = def {bar = 1}

testFooDef :: Spec
testFooDef =
    describe "foo" $
    it "Foo {bar = 1, baz = 0}" $ foo `shouldBe` Foo {bar = 1, baz = 0}
```

<!--
##### 型の内部構造を隠蔽できるかどうか
-->

##### Whether the internal structure of the type can be hidden

<!--
全く隠蔽していません．
-->

Not at all.

<!--
##### 不正な値の生成を防ぐことができるかどうか
-->

##### Whether the generation of illegal values can be prevented

<!--
防げません．利用者は以下のようなコードを容易に書くことが出来てしまいます．
-->

It cannot prevent it. Users can easily write codes such as the following.

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
##### その他
-->

##### Others

<!--
この方法は同時にセレクタ関数もエクスポートしてしまうため，名前空間を圧迫します．ただし，[`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html)を有効にすることでセレクタ関数の定義を削除することが出来ます．またこの拡張機能を有効にする際は，[`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html)や[`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html)，[`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html)を用いると，コードが書きやすくなります．Haskell Day 2021のfumieval氏の発表「[Haskell は別言語になりました――RecordDotSyntax と NoFieldSelectors](https://youtu.be/haZl-q6mfyk?t=2581)」も参考にしてください．
-->

This method also exports selector functions, which takes up too much namespaces. However, you can remove these selector functions by enabling [`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html). Also, enabling [`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html), [`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html), and [`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html) will make writing code easier when you enable `NoFieldSelector`. See the presentation ["Haskell has become a new language--RecordDotSyntax and NoFieldSelectors" (Haskell は別言語になりました――RecordDotSyntax と NoFieldSelectors)](https://youtu.be/haZl-q6mfyk?t=2581) by fumieval at Haskell Day 2021.

<!--
### [スマートコンストラクタ](https://wiki.haskell.org/Smart_constructors)を定義する
-->

### Define [smart constructors](https://wiki.haskell.org/Smart_constructors)

<!--
#### 概要
-->

#### Overview

<!--
データ構造は公開せず，代わりに引数を基に値を生成する関数を定義します．
-->

The data structure is not exposed; instead, define a function that generates a value based on the arguments.

<!--
Haskellではそのような関数を定義する際，[`mk`](https://kowainik.github.io/posts/naming-conventions)や[`from`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List-NonEmpty.html#v:fromList)という接頭辞をつけることが一般的のようです．後者は特に，引数が一つの場合に用いられます．
-->

In Haskell, it is common to add a prefix like [`mk`](https://kowainik.github.io/posts/naming-conventions) or [`from`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List-NonEmpty.html#v:fromList). The latter in particular is used when the number of parameters is one.

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
#### 評価
-->

#### Evaluation

<!--
##### 値の構築手段を提供するためのコードの量
-->

### The amount of code to provide a means of constructing a value

<!--
定義するコンストラクタの数や，コンストラクタ内の処理によりますが，比較的コード量は少ないと思います．
-->

Although it depends on the number of constructors defined and the processing within the constructors, the amount of code should be relatively small.

<!--
##### 値を構築するためのコードの量
-->

##### The amount of code to construct a value

<!--
エラー処理などを行う場合は多少長くなると思いますが，関数一つを呼ぶだけですので，こちらも他の方法と比較すると短いと思います．
-->

It should also be relatively small, although it will be a bit larger if it does extra processes like error handling.

<!--
##### 型の内部構造を隠蔽できるかどうか
-->

##### Whether the internal structure of the type can be hidden

<!--
型の名前とコンストラクタ関数のみを公開すればよいため，型の内部構造は隠蔽できます．
-->

You can hide the internal structure of a type because you only need to expose the type name and constructor functions.

<!--
##### 不正な値の生成を防ぐことができるかどうか
-->

##### Whether the generation of illegal values can be prevented

<!--
上記の例のように，受け取った引数を精査することで，不正な値が生成されようとしたときに`Nothing`などを返すことが出来ます．
-->

As shown in the example above, constructor functions can check the received arguments and return something like `Nothing` if the caller attempts to generate an invalid value.

<!--
場合によっては`error`を呼び出して直ちにプログラムを終了させることも出来ます．
-->

It is also possible to terminate the program immediately by invoking `error`.

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
    | age < 0 = error "The age is negative."
    | otherwise = Person {..}

testPanicOnEmptyName :: Spec
testPanicOnEmptyName =
    describe "mkPerson'" $
    it "prints the error message \"An empty name is passed.\" and terminates the program if it receives an empty name." $
    evaluate (mkPerson' "" 1) `shouldThrow` errorCall "An empty name is passed."
```

<!--
あるいは，エラー型を用いてエラーを呼び出し側に通知することもできます．
-->

Or, you can use error types to notify the caller of an error.

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
    it "returns a `Left NegativeAge` if it receives a negative age." $
    mkPerson'' "Tom" (-3) `shouldBe` Left NegativeAge
```

<!--
##### その他
-->

##### Others

<!--
状況に応じて複数のコンストラクタ関数を定義することも可能です．
-->

Depending on the situation, you can define multiple constructor functions.

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
    it "generates a `Person` whose age is 100 years old." $
    mkLongevity "Tom" `shouldBe` Right Person {name = "Tom", age = 100}
```

<!--
この方式の欠点として，コンストラクタ関数の引数が増えるとコードが読みづらくなることを挙げておきます．
-->

One drawback of this method is that the code becomes more difficult to read as the number of arguments to the constructor function increases.

<!--
### Builderパターンを用いる
-->

### Using the Builder Pattern

<!--
#### 概要
-->

#### Overview

<!--
まず，値を構築するための別の型を用意します．次に，その型の値に対して関数を用いて，どのような値を生成するかという設定を施します．最後にその値から，本来生成したかった型の値を生成します．
-->

First, define a different type to construct a value from. Next, apply functions to the values of that type to determine what value to generate. Finally, generate a value of the desired type from the value.

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

<!--
#### 評価
-->

#### Evaluation

<!--
##### 値の構築手段を提供するためのコードの量
-->

##### The amount of code to provide a means of constructing a value

<!--
関数の数にもよりますが，スマートコンストラクタを定義するよりもかなり量が多くなると思います．
-->

Depending on the number of functions, it may become considerably bigger than defining a smart constructor.

<!--
##### 値を構築するためのコードの量
-->

### The amount of code to construct a value

<!--
こちらも使用する関数の数によりますが，やはり単にコンストラクタ関数を呼び出すよりも量が多くなります．
-->

This also depends on the number of functions used, but again, the amount is greater than simply calling a constructor function.

<!--
##### 型の内部構造を隠蔽できるかどうか
-->

##### Whether the internal structure of the type can be hidden

<!--
型の名前と各関数のみを公開すればよいため，型の内部構造は隠蔽できます．
-->

Since we only need to expose type names and functions, we can hide the internal structure of the types.

<!--
##### 不正な値の生成を防ぐことができるかどうか
-->

##### Whether the generation of illegal values can be prevented

<!--
各関数内で値の精査を行うことで達成できます．
-->

It can be achieved by examining values inside each function.

<!--
### 値を構築するための型を定義し，その内部構造を公開する
-->

### Define a type to construct a value and expose its internal structure

<!--
#### 概要
-->

#### Introduction

<!--
Builderパターンと同じように，値を構築するための別の型を定義しますが，その型の内部構造を公開します．
-->

Define a separate type to construct a value, just like the Builder pattern, but expose the internal structure of the type.

<!--
#### コード例
-->

#### Code example

<!--
```haskell
data PersonBuilder' = PersonBuilder'
    { name :: String
    , age  :: Int
    }

mkPerson'''' :: PersonBuilder' -> Either PersonError Person
mkPerson'''' PersonBuilder' {..}
    | null name = Left EmptyName
    | age < 0 = Left NegativeAge
    | otherwise = Right Person {..}

loyter :: Either PersonError Person
loyter = mkPerson'''' PersonBuilder' {name = "ロイター", age = 32}

testLoyter :: Spec
testLoyter =
    describe "loyter" $
    it "`Right`値を返す" $ loyter `shouldBe` Right Person {name = "ロイター", age = 32}
```
-->

```haskell
data PersonBuilder' = PersonBuilder'
    { name :: String
    , age  :: Int
    }

mkPerson'''' :: PersonBuilder' -> Either PersonError Person
mkPerson'''' PersonBuilder' {..}
    | null name = Left EmptyName
    | age < 0 = Left NegativeAge
    | otherwise = Right Person {..}

loyter :: Either PersonError Person
loyter = mkPerson'''' PersonBuilder' {name = "Loyter", age = 32}

testLoyter :: Spec
testLoyter =
    describe "loyter" $
    it "returns a `Right` value" $
    loyter `shouldBe` Right Person {name = "Loyter", age = 32}
```

<!--
#### 評価
-->

#### Evaluation

<!--
##### 値の構築手段を提供するためのコードの量
-->

##### The amount of code to provide a means of constructing a value

<!--
型によります．値コンストラクタが持つ引数やレコードのフィールド数が大きくなるとそれだけコードは長くなりますが，Builderパターンよりは短くなると思います．
-->

It depends on the type. The code will be longer as the number of parameters of a value constructor or the number of fields in a record increases. However, the code should be shorter than that of the Builder Pattern.

<!--
##### 値を構築するためのコードの量
-->

### The amount of code to construct a value

<!--
こちらも使用する関数の数によりますが，やはり単にコンストラクタ関数を呼び出すよりも量が多くなります．
-->

This also depends on the type, but it should be shorter than the Builder pattern.

<!--
##### 型の内部構造を隠蔽できるかどうか
-->

##### Whether the internal structure of the type can be hidden

<!--
最終的に生成される値の型については隠蔽できますが，その生成のために用いる型に関しては公開しています．
-->

It can be hidden for the type of the final generated value, but for the type used to generate it, it is exposed.

<!--
##### 不正な値の生成を防ぐことができるかどうか
-->

##### Whether the generation of illegal values can be prevented

<!--
最終的な値を生成する際に，その元となる型の値（上記における`PersonBuilder'`）を精査することで不正な値の生成を防ぐことはできます．ただし，生成元の値に関しては途中で不正な値となり得ます．一見問題なさそうですが，利用者が型を変換せずに値を使用することがないよう，注意する必要があります．
-->

It is possible to prevent the generation of illegal values by examining the value of the source type (`PersonBuilder'` in the above example) when the final value is being generated. However, the source value can become invalid in the process. Although it does not seem to be a problem at first glance, you should be careful not to let users use the value before converting it to the final type.
