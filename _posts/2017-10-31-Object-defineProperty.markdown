---
layout: post
title:  "Object.defineProperty"
date:   2016-10-31
categories: js
---

### writable

比如说用普通的方法创建一个obj, obj={a: 2}, 用obj.a= XX的方法是可以修改a的值的

```js
obj = {a: 2}
obj.a = 3
obj.a // => 3
````

而用defineProperty定义，默认不可以修改

```js
Object.defineProperty(a, "property", {value: 'Property Value'})
a.property = "hello"
a // => Object {a: 1, property: "Property Value"}
```

要加上`writable: false`才可以


### enumerable

普通方法定义的obj,key都是可以enumerable的

```js
b = {a: 1, b: 3, c: 6}

for (var key in b){console.log(key)} // => a,b,c
```

用defineProperty定义的obj, key都是不能enumerable的, 在chrome console中显示是半透明的

```js
lala = {}
Object.defineProperty( lala, "hideme", {value: 'shit'})

for (var key in lala){console.log(key)} // => undefined
```

想要enumerable,需要加上`enumerable: true`

```js
Object.defineProperty(obj, "hideme", {enumerable: true});
```

### configurable

之前用`defineProperty`创建的`obj`可以被后来的`defineProperty`重新定义而被修改掉

```js
var obj = {};
Object.defineProperty(obj, "const", {
  value: "const val", writable: false, configurable: true}
);

// 修改不了
obj.const = "new val";
alert(obj.const); // "const val"

// 这里重新定义 `key const` 为 `writable:true`
Object.defineProperty(obj, "const", {writable: true});

// 那么这个key就可以修改了
obj.const = "overridden value";
alert(obj.const); // "overridden value"
```

```js
var obj = {};
Object.defineProperty(obj, "const", 
  {value: "const val", writable: false, configurable: false}
);

obj.const = "new val";
alert(obj.const); // "const val"
 
try {
  Object.defineProperty(obj, "const", {writable: true}); // CANNOT BE RE-DEFINED!
} catch (e) {
  alert(e);
}
obj.const = "overridden value";
alert(obj.const); // "const val"
```

###  用defineProperty重新定义key

```js
a = {b: 1} // a.b=XXX是可以修改的b的值的

// 可以重新定义key b的值
// 并且不会像使用`defineProperty`新创建的key那样，不能修改，不能a.b = XXX
Object.defineProperty(a, "b", {value: "shit"}) 

a.b // => shit

a.b = "lala"
a.b // => "lala"
```

### Property Accessors

```js
var obj = {};
Object.defineProperty(obj, "property", 
    {
      get:function() { alert('get value'); }, 
      set:function(val) { alert('set value'); }
    });
var temp = obj.property; // alerts "get value"
obj.property = ""; // alerts "set value"
```

### obj中定义一个方法，想调用方法却不想写成`obj.key()`，因为不想要`()`怎么办？

```js
// original object
var player = {
    displayText: "<span>you</span>",
    currentPosition: 0,
    level: 1,
    // health: function() { return 10 + (this.level * 15) },
    strength: function() { return this.level * 5 },
    hitRating: 4
}
```

在object中定义一个value是function的话，调用需要object.key(),加括号才可以调用。

如果就是不想加括号

```
// create property with accessor method
Object.defineProperty(player, "health", {
    get: function () {
        return 10 + (player.level * 15)
    }
})

// call the property
alert(player.health);  // 25
player.level++;
alert(player.health);  // 40
```
