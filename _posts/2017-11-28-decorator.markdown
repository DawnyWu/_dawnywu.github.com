---
layout: post
title:  "decorator"
date:   2016-11-28
categories: js
---

### Decorating a class

a decorator takes the target constructor

```js
class Person {
  name() { return `${this.first} ${this.last}` }
}

// 内部原理

Object.defineProperty(Person.prototype, 'name', {
  value: specifiedFunction,
  enumerable: false,
  configurable: true,
  writable: true
});

```

我们希望实现

```js
class Person {
  @readonly
  name() { return `${this.first} ${this.last}` }
}
```