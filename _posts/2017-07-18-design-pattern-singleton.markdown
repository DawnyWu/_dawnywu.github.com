---
layout: post
title:  "Design Patterns-Singleton"
date:   2016-07-18
categories: js
---

### Singleton

#### 方法一：把singleton instance写在构造函数对象上

缺点： 别人可以修改Universe.instanced对象

```js
function Universe() {
  // do we have an existing instance?
  if (typeof Universe.instance === "object") {
    return Universe.instance; 
  }
  // proceed as normal 
  this.start_time = 0; this.bang = "Big";
  // cache 
  Universe.instance = this;
  // implicit return:
  // return this; 
}
// testing
var uni = new Universe();
var uni2 = new Universe();
uni === uni2; // true
```


#### 方法二：用closure

缺点：由于使用了Self-Defining函数，之前函数的property就被覆盖掉了

```js
function Universe() {
  // the cached instance
  var instance = this;
  // proceed as normal 
  this.start_time = 0; this.bang = "Big";
  // rewrite the constructor
  Universe = function () {
    return instance; };
  }
```

缺点示例：

```js
// adding to the prototype 
Universe.prototype.nothing = true;
var uni = new Universe();

Universe.prototype.everything = true;
var uni2 = new Universe();
// 由于是singleton, un1 === un2,都有nothing，但是没有everything的

//Test
uni.nothing; // true 
uni2.nothing; // true 
uni.everything; // undefined 
uni2.everything; // undefined

// that sounds right: 
uni.constructor.name; // "Universe"
// constructor名字没有变的原因是新旧函数都叫Universe

// but that's odd:
uni.constructor === Universe; // false
// 是因为已经不是同一个对象了，uni指向旧的对象
```

如果想要`uni.constructor === Universe`是`true`的话, 需要让第一次生成的instance的constructor指向`redefined`的Universe

```js
function Universe() {
  // the cached instance
  var instance;
  // rewrite the constructor
  Universe = function Universe() {
               return instance; 
             };
  // carry over the prototype properties
  Universe.prototype = this;
  // the instance
  instance = new Universe();
  // reset the constructor pointer
  instance.constructor = Universe;
  // all the functionality 
  instance.start_time = 0; instance.bang = "Big";
  return instance; 
}
```

下边代码也可以解决刚才的问题，他根本没用 Self-Defining Functions

```js
var Universe; 
(function () {
  var instance;
  Universe = function Universe() {
    if (instance) {
      return instance;
    }
    instance = this;
    // all the functionality 
    this.start_time = 0; this.bang = "Big";
  }; 
}());
```


****************


***必要知识 Self-Defining Functions***


```js
var scareMe = function () {
  alert("Boo!");
  scareMe = function () {
    alert("Double boo!"); 
  };
};
// using the self-defining function scareMe(); // Boo!
scareMe(); // Double boo!
```

要注意，重新定义的函数会覆盖掉之前函数的property

把函数复制给一个变量，或作为一个object的value后，多次运行函数。虽然函数会覆盖掉，但是变量或key仍会指向旧的函数，

```js
// 1. adding a new property
scareMe.property = "properly";
// 2. assigning to a different name
var prank = scareMe;
// 3. using as a method 
var spooky = {
      boo: scareMe 
    };
// calling with a new name
prank(); // "Boo!"
prank(); // "Boo!" 
console.log(prank.property); // "properly"
// calling as a method
spooky.boo(); // "Boo!"
spooky.boo(); // "Boo!"
console.log(spooky.boo.property); // "properly"
// using the self-defined function 
scareMe(); // Double boo!
scareMe(); // Double boo! 
console.log(scareMe.property); // undefined
```




