---
layout: post
title:  "speakingjs"
date:   2016-11-16
categories: js
---

### expression 和 statement的区别

An *expression* produces a value and can be written wherever a value is expected

Roughly, a *statement* performs an action. Loops and if statements are examples of statements. A program is basically a sequence of statements

Wherever JavaScript expects a statement, you can also write an expression. Such a statement is called an expression statement. The reverse does not hold 

if statement

```js
var salutation;
if (male) {
    salutation = 'Mr.';
} else {
    salutation = 'Mrs.';
}
```

conditional operator 是 expression

```js
var salutation = (male ? 'Mr.' : 'Mrs.');
```

In order to prevent ambiguity during parsing, JavaScript does not let you use object literals and function expressions as statements. 

expression statements must not start with:

* A curly brace

* The keyword function

***例子***

eval parses its argument in statement context.

```js
eval('{a: 1}') // => 1
eval('({ foo: 123 })') // => {foo: 123}
```
