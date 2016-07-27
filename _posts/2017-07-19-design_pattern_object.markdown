---
layout: post
title:  "Design Patterns-Object Creation"
date:   2016-07-19
categories: js
---

### object private member

```js
function Gadget() {
  // private member
  var name = 'iPod';
  // public function 
  this.getName = function () {
    return name; 
  };
}

var toy = new Gadget();
// `name` is undefined, it's private 
console.log(toy.name); // undefined
```

上边的`getName`可以叫做`Privileged Methods`, because it has “special” access to the private property name.

### private member 失败的情况

```js
function Gadget() { 
  // private member 
  var specs = {
    screen_width: 320, screen_height: 480, color: "white"
  };
  // public function 
  this.getSpecs = function () {
    return specs; 
  };
}
```

`private member`是`object`, `getSpecs`返回`private member`的索引，从而可以修改`private member`...

```js
var toy = new Gadget(), 
specs = toy.getSpecs();

specs.color = "black"; 
specs.price = "free";
console.dir(toy.getSpecs());
```

The `solution` to this unexpected behavior is to be careful not to pass references to objects and arrays you want to keep private

比如不返回原对象的索引，而是按需要返回其中的几个属性`getDimension`，或返回shallowCopy,deepCopy

### Object Literals 如何创建 private member

```js
var myobj; // this will be the object 
(function () {
  // private members
  var name = "my, oh my";
  // implement the public part 
  // note -- no `var`
  myobj = {
    // privileged method 
    getName: function () {
      return name; 
    }
  }; 
}());
myobj.getName(); // "my, oh my"
```

```js
var myobj = (function () { 
  // private members
  var name = "my, oh my";
  // implement the public part 
  return {
    getName: function () {
      return name; 
    }
  }; 
}());
myobj.getName(); // "my, oh my"
```

### 创建private member缺点及解决

比如下边这种，每一次new都会重新生成var name="ipad"

```js
function Gadget() {
  // private member
  var name = 'iPod';
  // public function 
  this.getName = function () {
    return name; 
  };
}
```

```js
function Gadget() {
  // private member
  var name = 'iPod';
  // public function 
  this.getName = function () {
    return name; 
  };
}

Gadget.prototype = (function () { 
  // private member
  var browser = "Mobile Webkit"; 
  // public prototype members 
  return {
    getBrowser: function () {
      return browser; 
    }
  }; 
}());

```

### Revealing Private Functions As Public Methods

让`Private Function`表现得像`Public Methods`, 在一些情况下也是有用的

```js
var myarray;
(function () {
  var astr = "[object Array]",
      toString = Object.prototype.toString;

  function isArray(a) {
    return toString.call(a) === astr; 
  }

  function indexOf(haystack, needle) { 
    var i = 0,
        max = haystack.length; 
    for (; i < max; i += 1) {
      if (haystack[i] === needle) {
        return i; 
      }
    }
    return −1; 
  }

  myarray = {
    isArray: isArray, 
    indexOf: indexOf, 
    inArray: indexOf
  };
}());
```

如果有人把`indexOf`设为`null`,对`inArray`方法毫无影响

### Declaring Dependencies

```js
var myFunction = function () { 
  // dependencies
  var event = YAHOO.util.Event, 
      dom = YAHOO.util.Dom;

  // use event and dom variables
  // for the rest of the function... 
};
```

### Module Pattern

```js
MYAPP.namespace('MYAPP.utilities.array'); 
MYAPP.utilities.array = (function () {
  // dependencies
  var uobj = MYAPP.utilities.object,
      ulang = MYAPP.utilities.lang,

      // private properties 
      array_string = "[object Array]", 
      ops = Object.prototype.toString;

      // private methods 
      // ...

      // end var

  // optionally one-time init procedures 
  // ...

  // public API 
  return {
    inArray: function (needle, haystack) {
      for (var i = 0, max = haystack.length; i < max; i += 1) {
      if (haystack[i] === needle) {
      return true; }
      } 
    },
    isArray: function (a) {
      return ops.call(a) === array_string; 
    }
    // ... more methods and properties 
  };
}());
```

#### Revealing Module Pattern

```js
MYAPP.utilities.array = (function () {
  // private properties
  var array_string = "[object Array]",
  ops = Object.prototype.toString,
  // private methods
  inArray = function (haystack, needle) {
    for (var i = 0, max = haystack.length; i < max; i += 1) { 
      if (haystack[i] === needle) {
        return i; 
      }
    }
    return −1; 
  },
  isArray = function (a) {
    return ops.call(a) === array_string; 
  };
  // end var
  // revealing public API
  return {
    isArray: isArray,
    indexOf: inArray 
  };
}());
```





#### Importing Globals into a Module

```js
MYAPP.utilities.module = (function (app, global) {
  // references to the global object
  // and to the global app namespace object 
  // are now localized
}(MYAPP, this));
```

### Static Members

#### Private Static Members

* Shared by all the objects created with the same constructor function
* Not accessible outside the constructor


```js
var Gadget = (function () {
  // static variable/property 
  // private counter 
  var counter = 0;
  // returning the new implementation 
  // of the constructor
  return function () {
    console.log(counter += 1); 
  };
}()); // execute immediately

var g1 = new Gadget(); // logs 1 
var g2 = new Gadget(); // logs 2 
var g3 = new Gadget(); // logs 3
```

好像可以用来做instance的id,我们来做一个previlege method

```js
// constructor
var Gadget = (function () {
  // static variable/property 
  var counter = 0,
      NewGadget;
  // this will become the
  // new constructor implementation 
  NewGadget = function () {
    counter += 1; 
  };
  // a privileged method 
  NewGadget.prototype.getLastId = function () {
    return counter; 
  };
  // overwrite the constructor 
  return NewGadget;
}()); // execute immediately

var iphone = new Gadget(); 
iphone.getLastId(); // 1 
var ipod = new Gadget(); 
ipod.getLastId(); // 2 
var ipad = new Gadget(); 
ipad.getLastId(); // 3
```

### Object Constants

```js
// constructor
var Widget = function () {
  // implementation... 
};
// constants 
Widget.MAX_HEIGHT = 320; 
Widget.MAX_WIDTH = 480;
```

我们需要MAX_HEIGHT不可变

you can create a private property and provide a getter method, but no setter. This is probably overkill in many cases when you can get by with a simple convention, but it is still an option.

The following example is an implementation of a general-purpose constant object

```js
var constant = (function () {
  var constants = {},
      ownProp = Object.prototype.hasOwnProperty, 
      allowed = {
        string: 1, number: 1, boolean: 1
      },
  prefix = (Math.random() + "_").slice(2); 
  return {
    set: function (name, value) { 
      if (this.isDefined(name)) {
        return false; 
      }
      if (!ownProp.call(allowed, typeof value)) {
        return false; 
      }
      constants[prefix + name] = value;
      return true;
    },
    isDefined: function (name) {
      return ownProp.call(constants, prefix + name); 
    },
    get: function (name) {
      if (this.isDefined(name)) {
        return constants[prefix + name]; 
      }
      return null; 
    }
  }; 
}());

// check if defined 
constant.isDefined("maxwidth"); // false
// define
constant.set("maxwidth", 480); // true
// check again 
constant.isDefined("maxwidth"); // true
// attempt to redefine 
constant.set("maxwidth", 320); // false
// is the value still intact? 
constant.get("maxwidth"); // 480
```















