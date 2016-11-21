---
layout: post
title:  "Object Keys"
date:   2016-11-17
categories: js
---


### Kinds of Properties

object 可以有3种property

* Properties (or named data properties)

* Accessors (or named accessor properties)

* Internal properties


***想要删除一个obj的property***

```js
delete obj.hello
delete obj['not an identifier']
```
delete affects only the direct (“own,” noninherited) properties of an object. 

#### Function.prototype.bind(thisValue, arg1?, ..., argN?)

```js
function func() {
    console.log('this: '+this);
    console.log('arguments: '+Array.prototype.slice.call(arguments));
}
var bound = func.bind('abc', 1, 2);

```

```shell
> bound(3)
this: abc
arguments: 1,2,3
```

### this shadow

forEach中的this是global。。。。

```js
var obj = {
    name: 'Jane',
    friends: [ 'Tarzan', 'Cheeta' ],
    loop: function () {
        'use strict';
        this.friends.forEach(
            function (friend) {  // (1)
                console.log(this.name+' knows '+friend);  // (2)
            }
        );
    }
};
```

