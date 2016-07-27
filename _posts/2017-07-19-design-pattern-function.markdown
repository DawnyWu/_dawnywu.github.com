---
layout: post
title:  "Design Patterns-Function"
date:   2016-07-19
categories: js
---


### Init-Time Branching

```js
// BEFORE
var utils = {
  addListener: function (el, type, fn) {
    if (typeof window.addEventListener === 'function') {
      el.addEventListener(type, fn, false);
    } else if (typeof document.attachEvent === 'function') { // IE
      el.attachEvent('on' + type, fn);
    } else { // older browsers
      el['on' + type] = fn; 
    }
  },
  removeListener: function (el, type, fn) {
  // pretty much the same... 
  }
};
```

这样每一次运行都要走一遍if语句

修改为在不同情况下定义不同的`utils`,这样只定义一遍，以后不需要再变动

```js
// AFTER
// the interface 
var utils = {
  addListener: null,
  removeListener: null 
};
// the implementation
if (typeof window.addEventListener === 'function') {
  utils.addListener = function (el, type, fn) {
    el.addEventListener(type, fn, false); 
  };
  utils.removeListener = function (el, type, fn) {
    el.removeEventListener(type, fn, false); 
  };
} else if (typeof document.attachEvent === 'function') {// IE 
  utils.addListener = function (el, type, fn) {
    el.attachEvent('on' + type, fn); 
  };
  utils.removeListener = function (el, type, fn) {
    el.detachEvent('on' + type, fn); 
  };
} else { // older browsers
  utils.addListener = function (el, type, fn) {
    el['on' + type] = fn; 
  };
  utils.removeListener = function (el, type, fn) {
    el['on' + type] = null; 
  };
}
```

### Function Properties—A Memoization Pattern

```js
var myFunc = function (param) {
  if (!myFunc.cache[param]) {
    var result = {};
    // ... expensive operation ... 
    myFunc.cache[param] = result;
  }
  return myFunc.cache[param]; 
};

// cache storage 
myFunc.cache = {};
```

如果function有多个参数

```js
var myFunc = function () {
  var cachekey = JSON.stringify(Array.prototype.slice.call(arguments)),
      result;
  if (!myFunc.cache[cachekey]) { 
    result = {};
    // ... expensive operation ...
    myFunc.cache[cachekey] = result; 
  }
  return myFunc.cache[cachekey]; 
};

// cache storage 
myFunc.cache = {};
```

函数内部引用函数名用也可以用`arguments.callee`,不过在`ECMAScript 5 strict mode`不支持

### Curry

```js
// a curried add()
// accepts partial list of arguments 
function add(x, y) {
  var oldx = x, oldy = y;
  if (typeof oldy === "undefined") { 
    // partial
    return function (newy) {
             return oldx + newy; 
           };
  }
  // 传入两个参数的情况
  return x + y;
}

// a curried add
// accepts partial list of arguments 
function add(x, y) {
  if (typeof y === "undefined") { // partial 
    return function (y) {
      return x + y; };
    }
  // full application 
  return x + y;
}
```

a general-purpose way of making any function curried

```js
function schonfinkelize(fn) {
  var slice = Array.prototype.slice,
  // slice截断0~1，返回剩下的，不改变arguments
  // (add, 6) 保留住 6
  stored_args = slice.call(arguments, 1); 
  return function () {
    // 新的参数
    var new_args = slice.call(arguments), 
    // 新旧参数合并
    args = stored_args.concat(new_args);
    return fn.apply(null, args); 
  };
}

//schonfinkelize(add, 6)(7); // 13
```

### callback pattern

```js
// 遍历nodes,找到需要的nodes并返回
var findNodes = function () {
  var i = 100000, // big, heavy loop
      nodes = [], // stores the result
      found; // the next node found 
  while (i) {
    i -= 1;
    // complex logic here... 
    nodes.push(found);
  }
  return nodes; 
};

// 接收nodes,遍历所有并添加display none
var hide = function (nodes) {
  var i = 0, max = nodes.length; 
  for (; i < max; i += 1) {
    nodes[i].style.display = "none"; 
  }
};

// executing the functions 
hide(findNodes());
```

这样找到的`nodes`遍历了两遍，如果在`findNodes`的时候就调用`hide`,效率会提高很多

```js
// refactored findNodes() to accept a callback 
var findNodes = function (callback) {
  var i = 100000, nodes = [],
  found;

  // check if callback is callable
  if (typeof callback !== "function") {
    callback = false; 
  }
  while (i) { 
    i -= 1;
    // complex logic here...
    // now callback: 
    if (callback) {
      callback(found); 
    }
    nodes.push(found); 
  }
  return nodes; 
};

// 也可以用匿名函数
findNodes(function (node) {
node.style.display = "block"; });
```




