---
layout: post
title:  "Programming in Haskell"
date:   2016-10-20
categories: haskell
---

### Type 

a type is a collection of related values. 

### Class

a class is a collection of types that support certain overloaded operations called methods. 


### The difference between normal functions and constructor functions is 

is that the latter have no defining equations, and exist solely for the purpose of building piecesofdata.

For example,whereas the expression `negate 1.0` can be evaluated to `−1.0` by applying the definition of negate, the expression `Circle 1.0` is already fully evaluated, and hence cannot be further simplified, because there are no defining equations for Circle. Rather, the expression `Circle 1.0` is just a piece of data, in the same way that `1.0` itself is just data.

### Functions

#### 普通定义

```haskell
even :: Integral a ⇒ a → Bool
evenn = n‘mod‘2==0
```
#### Conditional expressions

```haskell
signum :: Int → Int
signum n = if n <0 then−1 else
             if n == 0 then 0 else 1
```

#### Guarded equations

```haskell
signum n | n<0  = −1 
         | n==0 = 0
         | otherwise = 1
```         

### Pattern matching

```haskell
¬ :: Bool → Bool 
¬ False = True
¬ True = False

(∧) :: Bool → Bool → Bool
True  ∧ True  = True 
True  ∧ False = False 
False ∧ True  = False 
False ∧ False = False
```

### LAMBDA EXPRESSIONS

直接把参数写在后边就相当于给函数传递参数。。。

```haskell
(λx→x+x)2
```

#### 作用

1. they can be used to formalise the meaning of curried function definitions. 

```haskell
add x y = x + y
add = λx → ( λy → x + y )
```

2. 使得函数的定义更加明确

const 函数: 你传入一个a,返回一个函数，这个函数的返回值永远是a, 可是看函数定义并不是很明确

```haskell
const :: a→b→a 
const x = x
```

```haskell
const :: a → (b → a)
const x = λ_ → x
```

3. 就是省去起名字。。。

### Section

Functions such as `+` that are written between their two arguments are called `operators`.

In particular, any operator can be converted into a curried function that is written before its arguments by enclosing the name of the operator in parentheses, as in `(+) 1 2`.

this convention also allows one of the arguments to be included in the parentheses if desired, as in `(1+) 2` and `(+2) 1`

```haskell
div 7 2
```

可以写成

```haskell
7 'div' 2
```

```haskell
(⊕)   = λx -> (λy -> x⊕y) 
(x ⊕) = λy -> x ⊕ y
(⊕y)  = λx -> x ⊕ y
```

### Functional parsers


type Parser a = String → [(a, String)]


```haskell
return :: a → Parser a
return v = λinp → [(v,inp)]
-- 和下边效果一样的
result v inp = [(v, inp)]

failure :: Parser a
failure = λinp → []

item :: Parser Char
item = λinp → case inp of
                    []→[]
                    (x : xs) → [(x , xs)]
```
