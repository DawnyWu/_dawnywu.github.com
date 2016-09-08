---
layout: post
title:  "Haskell Monad"
date:   2016-09-02
categories: fp
---
https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/

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

