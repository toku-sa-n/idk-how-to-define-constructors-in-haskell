# Haskellでのコンストラクタの定義の方法が分からない

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)

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

## 評価

これから紹介する方法を，以下の観点で評価します．

### 記述量の短さ

当然ながら，基本的に短いほどよいです．

### データ構造を隠蔽しているかどうか

データ構造は基本的に隠蔽するべきです．公開した場合，その構造を変更するとそのデータ構造を使用しているすべてのコードを変更する必要があります．また，データ構造をライブラリとして公開している場合，ライブラリのバージョンを上げる必要があります．

### 不正な値の生成を防いでいるか

空の名前や負の年齢などの値域から外れている値や，同じ要素を複数個含む集合など，値が持つべき前提から逸脱しているような値はそもそも生成されるべきではありません．またそのような値を生成しようとした際は，エラーを返すなどとして適切に対処するべきです．

## 値を構築する様々な方法

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

### 型の内部構造を公開する

#### 概要

型の内部構造を，エクスポート一覧で`Foo(..)`などとしてモジュール外に公開します．

#### コード例

```haskell
data Person = Person
    { name :: String
    , age :: Int
    } deriving (Eq, Show)

lomias :: Person
lomias = Person {name = "ロミアス", age = 24}
```

#### 利点

- 一番コード量が少ない．
- コードがわかりやすい．

#### 欠点

- データ構造を変更すると，そのデータ構造を使用しているすべてのコードを変更する必要がある．
- データ構造をライブラリとして公開している場合，データ構造の変更は破壊的変更となり，バージョンを上げる必要がある．
- 妥当ではない値も生成できてしまう．

```haskell
invalidPerson :: Person
invalidPerson = Person {name = "", age = -1}
```

- セレクタ関数をエクスポートすることで，名前空間を圧迫する．ただしこの問題は，[`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html)や[`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html)，[`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html)を用いると，そこまで問題ではなくなる．Haskell Day 2021のfumieval氏の発表「[Haskell は別言語になりました――RecordDotSyntax と NoFieldSelectors](https://youtu.be/haZl-q6mfyk?t=2581)」も参考．

### [スマートコンストラクタ](https://wiki.haskell.org/Smart_constructors)を定義する

#### 概要

データ構造は公開せず，代わりに引数を基に値を生成する関数を定義します．

Haskellではそのような関数を定義する際，[`mk`という接頭辞をつけることが一般的](https://kowainik.github.io/posts/naming-conventions)のようです．

#### コード例

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

#### 利点

- データ構造の中身を公開する必要がないため，構造を変更しても他のコードを同様に変更したり，ライブラリのバージョンを上げる必要がない．（ただしコンストラクタ関数の意味やシグネチャを変更したら当然それらを行う必要はある）
- 無効なデータが生成されることを防ぐことができる．場合によっては`error`を呼び出して直ちにプログラムを終了させることができる．

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

あるいは，エラー型を用いてエラーを呼び出し側に通知することもできる．

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

- 状況に応じて複数のコンストラクタ関数を定義できる．

```haskell
mkLongevity :: String -> Either PersonError Person
mkLongevity name = mkPerson'' name 100

testMkLongevity :: Spec
testMkLongevity =
    describe "mkLongevity" $
    it "年齢が100歳の`Person`を生成する．" $
    mkLongevity "Tom" `shouldBe` Right Person {name = "Tom", age = 100}
```

#### 欠点

- コンストラクタ関数の引数が増えるとコードが読みづらくなる．

### Builderパターンを用いる

#### 概要

値を構築するための別の型を定義し，関数を用いて最終的な値を構築します．

#### コード例

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
