---
layout: post
title:  "Build my own angular: $eval $apply"
date:   2016-07-14
categories: js
---

#### $eval

```js
// spec
var result = scope.$eval(function(scope) {
  return scope.aValue;
});

var result = scope.$eval(function(scope, arg) { 
  return scope.aValue + arg;
}, 2);


Scope.prototype.$eval = function(expr, locals) { 
  return expr(this, locals);
};
```

#### $apply

```js
Scope.prototype.$apply = function(expr) { 
  try {
    return this.$eval(expr); 
  } finally {
    this.$digest(); 
  }
};
```

#### $evalAsync - Deferred Execution

angular中异步的两种常用方法`$timeout`和`$evalAsync`

The reason why `$evalAsync` is often preferrable to a `$timeout with zero delay` has to do with the browser event loop. 

This difference between `$timeout` and `$evalAsync` is especially significant when you want to prevent unnecessary rendering: Why let the browser render DOM changes that are going to be immediately overridden anyway?

`$evalAsync`会让函数延迟执行，但是会在`$digest`之前

我们需要一个地方存放延迟执行的这些函数：

```js
function Scope() { 
  this.$$watchers = [];
  this.$$lastDirtyWatch = null;
  //要开始了
  this.$$asyncQueue = [];
}
```

```js
Scope.prototype.$evalAsync = function(expr) {
  this.$$asyncQueue.push({scope: this, expression: expr});
};
```

修改`digest`

```js
Scope.prototype.$digest = function() {
  var ttl = 10;
  var dirty;
  this.$$lastDirtyWatch = null;
  do {
    // digest前先运行$$asyncQueue中异步的函数
    while (this.$$asyncQueue.length) {
      var asyncTask = this.$$asyncQueue.shift();
      asyncTask.scope.$eval(asyncTask.expression);
    }
    dirty = this.$$digestOnce(); 
    if (dirty && !(ttl--)) {
      throw "10 digest iterations reached"; 
    }
  } while (dirty); 
};
```

#### Scheduling $evalAsync from Watch Functions

这个还要再看看

#### Coalescing $apply Invocations - $applyAsync

#### Watching Several Changes With One Listener:
$watchGroup

```js
// spec
scope.$watchGroup([
  function(scope) { return scope.aValue; },
  function(scope) { return scope.anotherValue; }
  ], 
  function(newValues, oldValues, scope) { 
    gotNewValues = newValues;
    gotOldValues = oldValues;
  }
);

Scope.prototype.$watchGroup = function(watchFns, listenerFn) { 
  var self = this;
  _.forEach(watchFns, function(watchFn) {
    self.$watch(watchFn, listenerFn);
  });
};
```







