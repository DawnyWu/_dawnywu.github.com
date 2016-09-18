---
layout: post
title:  "Haskell Monad"
date:   2016-09-02
categories: fp
---
https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/

http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

```js
var sine = function(x) { return Math.sin(x) };

var cube = function(x) { return x * x * x };

var sineCubed = cube(sine(x));

// compose 函数,先g后f
var compose = function(f, g) {
  return function(x) {
    return f(g(x));
  };
};

var sineOfCube = compose(sine, cube);
var y = sineOfCube(x);
```

我现在要打出一些log信息来让我知道函数运行成功了,可是我是不能在函数中console.log的，会产生副作用，我只能把它当做函数结果返回

```js
var sine = function(x) {
  return [Math.sin(x), 'sine was called.'];
};

var cube = function(x) {
  return [x * x * x, 'cube was called.'];
};
```

可是我们发现sineOfCube不能再运行了,sine要int,得到的却是[int, string]

我们希望的结果是这样的

```haskell
compose(sine, cube)(3)
// -> [0.956, 'cube was called.sine was called.']
```

我们需要重新写一个compose

```js
var composeDebuggable = function(f, g) {
  return function(x) {
    var gx = g(x),      // e.g. cube(3) -> [27, 'cube was called.']
        y  = gx[0],     //                 27
        s  = gx[1],     //                 'cube was called.'
        fy = f(y),      //     sine(27) -> [0.956, 'sine was called.']
        z  = fy[0],     //                 0.956
        t  = fy[1];     //                 'sine was called.'

    return [z, s + t];
  };
};

composeDebuggable(sine, cube)(3)
// -> [0.956, 'cube was called.sine was called.']
```

```js
var bind = function(f) {
  return function(tuple) {
    var x  = tuple[0],
        s  = tuple[1],
        fx = f(x),
        y  = fx[0],
        t  = fx[1];

    return [y, s + t];
  };
};
```

### lift bind unit

* `lift`, which converts a 'simple’ function into a debuggable function
* `bind`, which converts a debuggable function into a composable form
* `unit`, which converts a simple value into the format required for debugging, by placing it in a container

***unit***

```js
// unit :: Number -> (Number,String)
var unit = function(x) { return [x, ''] };

var f = compose(bind(sine), bind(cube));
f(unit(3)) // -> [0.956, 'cube was called.sine was called.']

// or ...
compose(f, unit)(3) // -> [0.956, 'cube was called.sine was called.']
```

***lift***

```js
// round :: Number -> Number
var round = function(x) { return Math.round(x) };

// roundDebug :: Number -> (Number,String)
var roundDebug = function(x) { return unit(round(x)) };
```

***bind***

bind, and its job is to take a `Number -> (Number,String)` function and return a `(Number,String) -> (Number,String)` function.

```js
var bind = function(f) {
  return function(tuple) {
    var x  = tuple[0],
        s  = tuple[1],
        fx = f(x),
        y  = fx[0],
        t  = fx[1];

    return [y, s + t];
  };
};
```

这样之前的compose函数就可以接着用了

### 另一个例子

```js
// children :: HTMLElement -> [HTMLElement]
var children = function(node) {
  var children = node.childNodes, ary = [];
  for (var i = 0, n = children.length; i < n; i++) {
    ary[i] = children[i];
  }
  return ary;
};

// e.g.
var heading = document.getElementsByTagName('h3')[0];
children(heading)
// -> [
//      "Translation from Haskell to JavaScript...",
//      <span class=​"edit">​…​</span>​
//    ]
```

那么我们想`grandchildren`应该是这样的

`var grandchildren = compose(children, children)`

可是`grandchildren`的输入输出是不同的，不是symmetric input out

我们自己手写个`grandchildren`

```js
// grandchildren :: HTMLElement -> [HTMLElement]
var grandchildren = function(node) {
  var output = [], childs = children(node);
  for (var i = 0, n = childs.length; i < n; i++) {
    output = output.concat(children(childs[i]));
  }
  return output;
};
```

```js
// unit :: a -> [a]
var unit = function(x) { return [x] };

// bind :: (a -> [a]) -> ([a] -> [a])
var bind = function(f) {
  return function(list) {
    var output = [];
    for (var i = 0, n = list.length; i < n; i++) {
      output = output.concat(f(list[i]));
    }
    return output;
  };
};
```

```js
var div = document.getElementsByTagName('div')[0];
var grandchildren = compose(bind(children), bind(children));

grandchildren(unit(div))
// -> [<h1>…</h1>, <p>…</p>, ...]
```

### Haskell符号

***fmap***

单纯的函数如+3是不能运用在Container中的data上的，用fmap就可以了

`fmap (+3) Just(2)`

符号如下

```haskell
getPostTitle <$> (findPost 1)
```

fmap可以应用在两个函数上，作用是`function composition`

`fmap (+3) (+1)`

***Applicatives***

它把function也封在Container中,可以对封在Container中的data使用

Applicative的符号是<*>

`Just (+3) <*> Just 2 == Just 5`

fmap可以做如下的事情


```haskell
-- 相当于fmap (+3) (Just 5)
> (+) <$> (Just 5)
Just (+5)
```

可是`fmap Just(+3) (Just 5)`这种就做不了了

```haskell
> Just (+5) <$> (Just 4)
ERROR ??? WHAT DOES THIS EVEN MEAN WHY IS THE FUNCTION WRAPPED IN A JUST
```

这时候就需要Applicative

```haskell
> (+) <$> (Just 5)
Just (+5)
> Just (+5) <*> (Just 3)
Just 8
````

```haskell
> (*) <$> Just 5 <*> Just 3
Just 15

-- 等价于这样

fmap * Just(5) => Just(*5)

Just(*5) <*> Just 3  => Just(15)
```

```haskell
> liftA2 (*) (Just 5) (Just 3)
Just 15
```

***Monad***

`>>=`叫做`bind`

```haskell
half x = if even x
           then Just (x `div` 2)
           else Nothing
```

`half`是一个函数，输入普通的值，输出`Container`

那要是输入`Container`呢？half函数肯定是不能处理的了

这时候我们就需要`>>=` `bind`

```haskell
> Just 3 >>= half
Nothing
> Just 4 >>= half
Just 2
> Nothing >>= half
Nothing
```

```haskell
> Just 20 >>= half >>= half >>= half
Nothing
```

### IO monad

有三个函数

```haskell
getLine :: IO String
readFile :: FilePath -> IO String
putStrLn :: String -> IO ()
```

他们都是输入普通数值，输出Container的,这样我们可以把它们串联在一起，用`>>=`

有语法糖可以用

```haskell
foo = do
    filename <- getLine
    contents <- readFile filename
    putStrLn contents
```

`functors`: you apply a function to a wrapped value using fmap or <$>

`applicatives`: you apply a wrapped function to a wrapped value using <*> or liftA

`monads`: you apply a function that returns a wrapped value, to a wrapped value using >>= or liftM

### Tuple List

A tuple is a fixed-size collection of values, where each value can have a different type. 

### Record Syntax

```haskell
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
```

```haskell
customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
```


















