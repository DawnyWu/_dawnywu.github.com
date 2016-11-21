---
layout: post
title:  "Haskell Moduel Import Export"
date:   2016-09-13
categories: Haskell
---

### Import

只import几个函数

```haskell
import Data.List (nub, sort)
```

不加载其中的某个函数

```haskell
import Data.List hiding (nub)  
```

如果模块中函数名字和prelude中方法重名,用如下方式,使用方法时`Data.Map.XXX`

```haskell
import qualified Data.Map  
```

或者

```haskell
import qualified Data.Map as M  
```

使用方法时可以`M.XXX`

[http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html](http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html) 中的例子

```haskell
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
```

### Export

```haskell
module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where
```

导出所有

```haskell
module ExportEverything where
```





