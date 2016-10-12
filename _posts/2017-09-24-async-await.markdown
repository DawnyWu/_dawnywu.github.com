---
layout: post
title:  "Async Await"
date:   2016-09-24
categories: Javascript
---

### Block means pulling

```js
price=getStockPrice("Denim Inc")
```

### Waiting means pushing

```js
getStockPrice("Denim Inc", (err, price) => {})
```

解决办法可以是Promise,但是用promise写代码的方式和block情况下写代码的方式不太一样，我们希望有一种类似的方式

### generator function

```js
function* getStockPrice(name){
  var symbol = yield getStockSymbol(name);
  var price = yield getSymbolPrice(symbol);
  return price
}
```

这种写法和block情况下写法基本类似，而且不会block

更好的消息是我们可以在循环里使用

### iteration

`iterator.next()`是pulling,consumer 通过`.next`拉取值

那么既然Generator生成Iterator,那它为什么叫Generator呢？不叫Iterator?

应为Generator不近包括Iterator,还包括Observer

### Async Iteration

```js
spawn(getStockPrice("Pfizer")).
  then(
     price => console.log(price)
    error => console.log(error)
  )
```


