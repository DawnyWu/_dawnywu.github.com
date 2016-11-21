---
layout: post
title:  "Haskell Parse JSON"
date:   2016-09-24
categories: Haskell
---

JSON有四种基本类型：`String`, `Number`, `Boolean`, `Null`


两种组合的类型`Array`, `Object`, `Object`的key永远是`string`

```haskell
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)
```  

我们现在可以把普通的haskell值变成JValue

那么如何反过来呢？我们使用pattern matching

```haskell
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing
```

```shell
*Main> getString (JString "hello")
Just "hello"
```

其他的get方法大同小异

```haskell
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull
```

### Printing JSON data

```haskell
module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)
```        

`intercalate`方法是把`list`插入`[list]`然后`flatten`

上面还是没有把JValue打出来

```haskell
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
```

### pretty print

`Doc type`和`text` `double` `string`方法我们定义在下边

```haskell
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
```

开发haskell的时候可以先用`stub`,具体定义可以以后再说

```haskell
import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
```

`undefined`的`type`是`a`

### Pretty printing a string

point free style

```haskell
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar
```

等于如下

```haskell
pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))
```

enclose, hcat, enclose方法都是我们下边定义的

```haskell
-- 把两个Doc类型的拼在一起
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined
```

```haskell
-- 用两个Char包裹住Doc
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
```

```haskell
-- 把数组内地Doc连接成Doc
hcat :: [Doc] -> Doc
hcat xs = undefined
```

### Arrays and objects, and the module header

```haskell
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
```                  

函数type定义了四个需要传入的参数，可是可以看到下边我们只传了3个



