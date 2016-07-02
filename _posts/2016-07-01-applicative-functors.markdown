---
layout: post
title:  "Applicative Functor"
date:   2016-06-30 
categories: FP
---
Container(add(2)) 想要作用在 Container(3) 上怎么办？

***ap***

ap is a function that can apply the function contents of one functor to the value contents of another. 


`Container.of(add(2)).ap(Container.of(3));`
`Container.of(2).map(add).ap(Container.of(3));`

```javascript
// this是Container(add(2))
Container.prototype.ap = function(other_container) {
  return other_container.map(this.__value);
}
```

***Applicative Functor***

An applicative functor is a pointed functor with an ap method

一条定律:

`F.of(x).map(f) == F.of(f).ap(F.of(x))`

比如

`Container(3).map(add(2)) = Container(add(3)).ap(Container(2))`

***Lift***

a pointfree way to write these applicative calls


```
var liftA2 = curry(function(f, functor1, functor2) {
  return functor1.map(f).ap(functor2);
});

var liftA3 = curry(function(f, functor1, functor2, functor3) {
  return functor1.map(f).ap(functor2).ap(functor3);
});
```
