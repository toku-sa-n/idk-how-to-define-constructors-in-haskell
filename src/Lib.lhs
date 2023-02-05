# Haskellでのコンストラクタの定義の方法が分からない

[![CI status](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions/workflows/ci.yml)

[English version is here.](README.en.md)

## はじめに

この記事では，Haskellにおいて，どのような手段で型の利用者にその型の値を構築させるかについて考察します．

## リポジトリ内のファイルについて

このリポジトリは，単一のStackライブラリとなっています．リポジトリのルートディレクトリで`stack test`を実行することで，このライブラリをテストすることができます．

[`src/Lib.lhs`](/src/Lib.lhs)では[markdown-unlit](https://github.com/sol/markdown-unlit)を用いており，[`README.md`](/README.md)はそのファイルへのシンボリックリンクです．ファイル内のすべてのHaskellコードブロックは連結しており，それらをすべて順番に結合したものが最終的なコードとなります．

なお，英語版のREADMEである[`README.en.md`](/README.en.md)は`.lhs`ファイルではありませんが，[`README.md`](/README.md)同様，すべてのコードブロックは連結しているものと見做してください．

## ライセンス

本文は[CC BY-SA 4.0](/LICENSE-CC-BY-SA)の下で利用可能です．またソースコードは[WTFPL](LICENSE-WTFPL)の下で利用可能です．

## バージョン情報

記事中のコードが常に意図するように動作することを確認するために，これらはGitHub Actions上で毎日最新のGHCやCabal，Stackを用いてコンパイル，実行されています．したがって，GHCやCabal，Stackのバージョンに関しては[Actionの実行結果](https://github.com/toku-sa-n/idk-how-to-define-constructors-in-haskell/actions)を，CIの詳細に関しては[.github/workflows/ci.yml](.github/workflows/ci.yml)を確認してください．

使用しているStackのレゾルバやライブラリのバージョンなどは[stack.yaml](stack.yaml)や[package.yaml](package.yaml)を確認してください．

## 評価

これから紹介する方法を，以下の観点で評価していきます．

### 値の構築手段を提供するためのコードの量

型自体の定義以外に必要とするコードの量です．基本的に短いほうが良いです．

### 値を構築するためのコードの量

用意された手段で値を構築しようとする際に必要とするコードの量です．こちらも基本的に短いほうが良いです．

### 型の内部構造を隠蔽できるかどうか

型の内部構造は基本的に隠蔽するべきです．公開した場合，その構造を変更するとそれに依存しているすべてのコードを変更する必要があります．また，生成された値があとから変更されてしまい，不正な値となってしまう恐れもあります．更に，型を内部構造も含めてライブラリとして公開している場合，構造を変更する際はライブラリのバージョンを上げる必要があります．

### 不正な値の生成を防ぐことができるかどうか

空の名前や負の年齢などの値域から外れている値や，同じ要素を複数個含む集合など，値が持つべき前提から逸脱しているような値はそもそも生成されるべきではありません．またそのような値を生成しようとした際は，エラーを返すなどとして適切に対処するべきです．

## 値を構築する様々な方法

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

### 型の内部構造を公開する

#### 概要

型の内部構造を，エクスポート一覧で`Foo(..)`などとしてモジュール外に公開します．

#### コード例

エクスポート一覧の例は上部にあります．

```haskell
data Person = Person
    { name :: String
    , age :: Int
    } deriving (Eq, Show)

lomias :: Person
lomias = Person {name = "ロミアス", age = 24}
```

#### 評価

##### 値の構築手段を提供するためのコードの量

単にエクスポート一覧で`Person(..)`などと書けばよいため，提供側のコード量は非常に少ないです．

##### 値を構築するためのコードの量

これは型によります．値コンストラクタが持つ引数やレコードのフィールド数が大きくなるとそれだけコードは長くなります．

ただし，予めデフォルト値を提供し，必要ならば利用者にその一部を変更してもらうという方法にすると，コードを削減できる場合があります．一例として，[`data-default`](https://hackage.haskell.org/package/data-default)を使用した方法を紹介します．

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

##### 型の内部構造を隠蔽できるかどうか

全く隠蔽していません．

##### 不正な値の生成を防ぐことができるかどうか

防げません．利用者は以下のようなコードを容易に書くことが出来てしまいます．

```haskell
invalidPerson :: Person
invalidPerson = Person {name = "", age = -1}
```

##### その他

この方法は同時にセレクタ関数もエクスポートしてしまうため，名前空間を圧迫します．ただし，[`NoFieldSelector`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html)を有効にすることでセレクタ関数の定義を削除することが出来ます．またこの拡張機能を有効にする際は，[`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html)や[`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html)，[`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html)を用いると，コードが書きやすくなります．Haskell Day 2021のfumieval氏の発表「[Haskell は別言語になりました――RecordDotSyntax と NoFieldSelectors](https://youtu.be/haZl-q6mfyk?t=2581)」も参考にしてください．

### [スマートコンストラクタ](https://wiki.haskell.org/Smart_constructors)を定義する

#### 概要

データ構造は公開せず，代わりに引数を基に値を生成する関数を定義します．

Haskellではそのような関数を定義する際，[`mk`](https://kowainik.github.io/posts/naming-conventions)や[`from`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List-NonEmpty.html#v:fromList)という接頭辞をつけることが一般的のようです．後者は特に，引数が一つの場合に用いられます．

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

#### 評価

##### 値の構築手段を提供するためのコードの量

定義するコンストラクタの数や，コンストラクタ内の処理によりますが，比較的コード量は少ないと思います．

##### 値を構築するためのコードの量

エラー処理などを行う場合は多少長くなると思いますが，関数一つを呼ぶだけですので，こちらも他の方法と比較すると短いと思います．

##### 型の内部構造を隠蔽できるかどうか

型の名前とコンストラクタ関数のみを公開すればよいため，型の内部構造は隠蔽できます．

##### 不正な値の生成を防ぐことができるかどうか

上記の例のように，受け取った引数を精査することで，不正な値が生成されようとしたときに`Nothing`などを返すことが出来ます．

場合によっては`error`を呼び出して直ちにプログラムを終了させることも出来ます．

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

あるいは，エラー型を用いてエラーを呼び出し側に通知することもできます．

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

##### その他

状況に応じて複数のコンストラクタ関数を定義することも可能です．

```haskell
mkLongevity :: String -> Either PersonError Person
mkLongevity name = mkPerson'' name 100

testMkLongevity :: Spec
testMkLongevity =
    describe "mkLongevity" $
    it "年齢が100歳の`Person`を生成する．" $
    mkLongevity "Tom" `shouldBe` Right Person {name = "Tom", age = 100}
```

この方式の欠点として，コンストラクタ関数の引数が増えるとコードが読みづらくなることを挙げておきます．

### Builderパターンを用いる

#### 概要

まず，値を構築するための別の型を用意します．次に，その型の値に対して関数を用いて，どのような値を生成するかという設定を施します．最後にその値から，本来生成したかった型の値を生成します．

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

#### 評価

##### 値の構築手段を提供するためのコードの量

関数の数にもよりますが，スマートコンストラクタを定義するよりもかなり量が多くなると思います．

##### 値を構築するためのコードの量

こちらも使用する関数の数によりますが，やはり単にコンストラクタ関数を呼び出すよりも量が多くなります．

##### 型の内部構造を隠蔽できるかどうか

型の名前と各関数のみを公開すればよいため，型の内部構造は隠蔽できます．

##### 不正な値の生成を防ぐことができるかどうか

各関数内で値の精査を行うことで達成できます．

### 値を構築するための型を定義し，その内部構造を公開する

#### 概要

Builderパターンと同じように，値を構築するための別の型を定義しますが，その型の内部構造を公開します．

#### コード例

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

#### 評価

##### 値の構築手段を提供するためのコードの量

型によります．値コンストラクタが持つ引数やレコードのフィールド数が大きくなるとそれだけコードは長くなりますが，Builderパターンよりは短くなると思います．

##### 値を構築するためのコードの量

こちらも型によりますが，やはりBuilderパターンよりは短くなると思います．

##### 型の内部構造を隠蔽できるかどうか

最終的に生成される値の型については隠蔽できますが，その生成のために用いる型に関しては公開しています．

##### 不正な値の生成を防ぐことができるかどうか

最終的な値を生成する際に，その元となる型の値（上記における`PersonBuilder'`）を精査することで不正な値の生成を防ぐことはできます．ただし，生成元の値に関しては途中で不正な値となり得ます．一見問題なさそうですが，利用者が型を変換せずに値を使用することがないよう，注意する必要があります．
