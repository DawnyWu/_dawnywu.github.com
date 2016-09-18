---
layout: post
title:  "Haskell Type"
date:   2016-09-14
categories: Haskell
---

我们希望`==`函数可以应用在各个不同类型上，而不是对比颜色的时候用`colorEq`,对比字符串的时候用`stringEq`

并且以后添加新的类型的时候，不需要改动旧的代码

我们使用`typeclass`来实现它

```haskell
class BasicEq a where
    isEqual :: a -> a -> Bool
```

上边我们定义了一个typeclass

下边我们让Bool成为这个typeclass的instance

```haskell
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False
```

这样Bool就可以使用isEqual方法了

我们再添加一个`isNotEqual`方法

```haskell
class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
```

定义class的function的时候，可以提供default

```haskell
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)
```    

### 应用：解决JSON问题

[http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html#library.jvalue](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html#library.jvalue)

我们写一个typeclass

```haskell
-- file: ch06/JSONClass.hs
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
```

```haskell
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
```


Haskell 98 不让我们按下边的方式写东西

```haskell
instance JSON String where
    toJValue               = JString

    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"
```

在源文件头写`{-# LANGUAGE TypeSynonymInstances #-}`就可以了



### Data Newtype 的不同

* Because a newtype's constructor is used only at compile time and does not even exist at runtime

```haskell
-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

-- bad: no fields
newtype TooFew = TooFew

-- bad: more than one field
newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
newtype TooManyCtors = Bad Int
                     | Worse Int
```                     

### JSON typeclasses without overlapping instances

```haskell
-- file: ch06/JSONClass.hs
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)
```    