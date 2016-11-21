---
layout: post
title:  "Write Yourself a Scheme"
date:   2016-10-20
categories: haskell
---

```haskell
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"
```

```haskell
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "No match" ++ show err
  Right val -> "Found value"
```

"lisp"是这个parser的名字。。。

在main中调用写好的parser

```haskell
main :: IO ()
main = do args <- getArgs
  putStrLn ( readExpr ( args !! 0) )
```

```haskell
spaces :: Parser () 
spaces = skipMany1 space
```

### liftM

```haskell
parseNumber : : Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
```

the result of many1 digit is actually a `Parser String`, so our combined `Number . read` still can’t operate on it. 

We need a way to tell it to just operate on the value inside the monad, giving us back a `Parser LispVal`.

### import Monad失败的问题

```haskell
import Control.Monad
```

### 未解决问题

```haskell
 parseString :: Parser LispVal
 parseString = do char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x

-- compile后
./eval "hello"
-- 输出
No match"lisp" (line 1, column 1):
unexpected "h"
expecting "\"" or digit
```

为什么通不过？digit从哪里来的？

```haskell
➜  03-evaluation git:(master) ✗ ./eval "saldkfjlkasjf"
No match: "lisp" (line 1, column 1):
unexpected "s"
expecting "\""
```



### Eval



```haskell
apply :: String -> [ LispVal ] ->  LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func
primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives :: [("+", numericBinop (+)),
               ("-", numericBinop (-)),
               ("*", numericBinop (*))]
```               

numericBinop : : ( Integer 


### Error

```haskell
data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
```

```haskell
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                            ++ " args; found values "
                                            ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected "
                                            ++ expected
                                            ++ ", found "
                                            ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
```