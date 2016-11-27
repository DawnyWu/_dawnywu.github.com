---
layout: post
title:  "destructuring and parameter handling"
date:   2016-11-23
categories: js
---

### destructuring

***Object patterns coerce values to objects***

`{}`匹配的时候会先把右边变为对象

需要注意的是，它用的不是Object()方法，而是内部的ToObject()方法，他们在处理`undefined`和`null`的时候不一样

Object() converts primitive values to wrapper objects and leaves objects untouched:

```js
typeof Object('abc')
// 'object'

var obj = {};
Object(obj) === obj
// true
```

```js
Object(undefined)
// {}
Object(null)
// {}
```

In contrast, `ToObject()` throws a TypeError

***default value***

```js
const [y = 5] = [3] // y=3
const [z = 5] = []  // z=5
```

```js
const [{ prop: x } = {}] = [];
```

`= {}`是做什么的呢？如果没有的话下边会报错

```js
const [{ prop: e }] = [];
// Uncaught TypeError: Cannot match against 'undefined' or 'null'.(…)
```

先进行匹配，发现没有数据，于是用`default value`

```js
{ prop: x } = {}
```

于是`x=undefined`



### parameter handling

*** ES6处理参数实际上是这样子

```js
function func(«FORMAL_PARAMETERS») {
    «CODE»
}

func(«ACTUAL_PARAMETERS»);

// is roughly equivalent to:

{
    let [«FORMAL_PARAMETERS»] = [«ACTUAL_PARAMETERS»];
    {
        «CODE»
    }
}

// example
function logSum(x=0, y=0) {
    console.log(x + y);
}

logSum(7, 8);

//becomes:

{
    let [x=0, y=0] = [7, 8];
    {
        console.log(x + y);
    }
}
```

***Named parameters via destructuring***

```js
function selectEntries({ start=0, end=-1, step=1 } = {}) { // (A)
    // The object pattern is an abbreviation of:
    // { start: start=0, end: end=-1, step: step=1 }

    // Use the variables `start`, `end` and `step` here
    ···
}

selectEntries({ start: 10, end: 30, step: 2 });
selectEntries({ step: 3 });
selectEntries({});
selectEntries();
```

`= {}` 的作用是`selectEntries()`的时候没有问题, 具体看上边`destructuring`


###  No more arguments!

```js
// ECMAScript 5: arguments
function logAllArguments() {
    for (var i=0; i < arguments.length; i++) {
        console.log(arguments[i]);
    }
}

// ECMAScript 6: rest parameter
function logAllArguments(...args) {
    for (const arg of args) {
        console.log(arg);
    }
}
```

#### 需要注意 undefined

undefined triggers the default value

```js
function f(x, y=0) {
  return [x, y];
}

f(1)
// [1, 0]

f()
// [undefined, 0]

f(undefined, undefined)
// [undefined, 0]
```

### Coding style tips

```js
function foo(requiredParam, optionalParam = undefined) {
    ···
}
```

use the default value undefined to make it obvious that the parameter is optional

***ensuring that a required parameter***

es5

```js
function foo(mustBeProvided) {
    if (arguments.length < 1) {
        throw new Error();
    }
    if (! (0 in arguments)) {
        throw new Error();
    }
    if (mustBeProvided === undefined) {
        throw new Error();
    }
    ···
}
```

es6可以

```js
function mandatory() {
    throw new Error('Missing parameter');
}
function foo(mustBeProvided = mandatory()) {
    return mustBeProvided;
}
Interaction:

> foo()
Error: Missing parameter
> foo(123)
123
```

