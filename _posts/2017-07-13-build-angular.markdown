---
layout: post
title:  "Build my own angular: Scope"
date:   2016-07-06
categories: js
---

### Scope

* Sharing data between controllers and views
* Sharing data between different parts of the application 
* Broadcasting and listening for events
* Watching for changes in data

一个`scope`只是一个普通的object,拥有`$watch`和`$digest`这两个函数

`$watch`接收`watcher`和`listener`函数

`watcher`函数返回监听的数据值，`angular`运行`$digest`函数，使用脏检查（对比新旧值是否有变化），若有变化运行`listener`函数

那么要实现`scope.$watch(watcher, listener)`函数，scope要可以保存`watcher`和`listener`,以便以后`scope.$digest`检查，调用

```js
Scope.prototype.$watch = function(watchFn, listenerFn) {
  var watcher = {
    watchFn: watchFn,
    listenerFn: listenerFn
  };
  this.$$watchers.push(watcher); 
};
```

```js
Scope.prototype.$digest = function() {
  var self = this;
  var newValue, oldValue;
  _.forEach(this.$$watchers, function(watcher) {
    newValue = watcher.watchFn(self); 
    oldValue = watcher.last;
    if (newValue !== oldValue) {
      watcher.last = newValue;
      watcher.listenerFn(newValue, oldValue, self);
    }
  }); 
};
```

之前`watch`返回的结果被保存在`watcher.last`里,那么就有个问题，第一次运行`$digest`的时候`watcher.last`是`undefined`啊。。。

```js
// 所以每个watcher加了last属性
Scope.prototype.$watch = function(watchFn, listenerFn) { 
  var watcher = {
    watchFn: watchFn,
    listenerFn: listenerFn,
    last: initWatchVal
  };

  this.$$watchers.push(watcher); 
};


Scope.prototype.$digest = function() {
  var self = this;
  var newValue, oldValue;
  _.forEach(this.$$watchers, function(watcher) {
    newValue = watcher.watchFn(self);
    oldValue = watcher.last;
    // 新旧值不同，调用listener
    if (newValue !== oldValue) {
      watcher.last = newValue;
      watcher.listenerFn(
        newValue,
        // 第一次$digest的时候要用newValue
        (oldValue === initWatchVal ? newValue : oldValue), 
        self
      ); 
    }
  });
};

```

#### 多个$watch之间有先后的情况

```js
it("triggers chained watchers in the same digest", function() {
  scope.name = 'Jane';
  scope.$watch(
    function(scope) { return scope.nameUpper; },
    function(newValue, oldValue, scope) {
      if (newValue) {
        scope.initial = newValue.substring(0, 1) + '.';
      }
    }
  );
  // 第一个$watch监听nameUpper变化，而运行下边这个$watch才会创建nameUpper属性，
  // 所以第一个`$watch`不会触发,这不是我们想要达到的效果
  scope.$watch(
    function(scope) { return scope.name; },
    function(newValue, oldValue, scope) {
      if (newValue) {
        scope.nameUpper = newValue.toUpperCase();
      } 
    }
  );
```
要解决上边的问题，我们要运行多次`$digest`,于是我们把原来的`$digest`改为`$$digestOnce`,返回`dirty` boolean值。 `$digest`函数现在要运行多次`$$digestOnce`

```js
Scope.prototype.$$digestOnce = function() {
  var self = this;
  var newValue, oldValue, dirty; 
  _.forEach(this.$$watchers, function(watcher) {
    newValue = watcher.watchFn(self);
    oldValue = watcher.last;
    if (newValue !== oldValue) {
      watcher.last = newValue;
      watcher.listenerFn(newValue,
        (oldValue === initWatchVal ? newValue : oldValue),
        self);
      // 新旧值不同，调用listener函数，把dirty设为true
      dirty = true;
    } 
  });
  return dirty;
};
```

现在的`dirty`

```js
Scope.prototype.$digest = function() { 
  var dirty;
  do {
    dirty = this.$$digestOnce(); 
  } while (dirty);
};
```

#### 多个$watch之间有互相watch的情况

```js
it("gives up on the watches after 10 iterations", function() {
  scope.counterA = 0;
  scope.counterB = 0;

  scope.$watch(
    function(scope) { return scope.counterA; },
    function(newValue, oldValue, scope) {
      scope.counterB++;
    }
  );

  scope.$watch(
    function(scope) { return scope.counterB; },
    function(newValue, oldValue, scope) {
      scope.counterA++;
    }
  );
```

这种情况会陷入死循环,所以我们要设置一个最大运行数`TTL(Time to live)`

```js
Scope.prototype.$digest = function() {
  var ttl = 10;
  var dirty;
  do {
    dirty = this.$$digestOnce();
    if (dirty && !(ttl--)) {throw "10 digest iterations reached"; }
  } while (dirty); 
};
```

### 优化Digest

一个`scope`有100个`watch`,只有一个`dirty`的`watch`,需要运行整整两遍，判断两百次新旧值是否一致

我们可以优化一下，记录下最后一个`dirty`的`watch`,下一次运行的时候，发现这个`watch`已经不`dirty`了，就不在运行其他`watch`了

```js
function Scope() { 
  this.$$watchers = [];
  this.$$lastDirtyWatch = null;
}
```

```js
Scope.prototype.$$digestOnce = function() {
  var self = this;
  var newValue, oldValue, dirty; 
  _.forEach(this.$$watchers, function(watcher) {
    newValue = watcher.watchFn(self);
    oldValue = watcher.last;
    if (newValue !== oldValue) {
      // 当新旧值不同的时候，self.$$lastDirtyWatch 设置为这个watch
      self.$$lastDirtyWatch = watcher;
      watcher.last = newValue;
      watcher.listenerFn(newValue,
        (oldValue === initWatchVal ? newValue : oldValue),
        self); 
      dirty = true;
      // 新旧值相同的时候，检查当前watch是不是$$lastDirtyWatch
    } else if (self.$$lastDirtyWatch === watcher) { 
      return false;
    } 
  });
  return dirty;
};
```

### Value-Based Dirty-Checking

目前检查新旧值是否相等是通过`===`，这个不能检查array或object中的值变化

所以当需要检查值是否相等时，我们使用`lodash`提供的`_.isEqual()`

我们保存值的方式也要改变，现在要存下`array`或`object`的`deep copy`,不能只存一个引用

由于这种检查方式会使用更多资源，angular默认不支持这种检查，需要你提供额外参数来使用这种功能

```js
Scope.prototype.$$digestOnce = function() {
  var self = this;
  var newValue, oldValue, dirty; 
  _.forEach(this.$$watchers, function(watcher) {
    newValue = watcher.watchFn(self);
    oldValue = watcher.last;
    if (!self.$$areEqual(newValue, oldValue, watcher.valueEq)) {
      self.$$lastDirtyWatch = watcher;
      // 如果有valueEq，说明是value-based check, watcher.last要记录deep copy
      watcher.last = (watcher.valueEq ? _.cloneDeep(newValue) : newValue);
      watcher.listenerFn(newValue,
        (oldValue === initWatchVal ? newValue : oldValue),
        self);
      dirty = true;
    } else if (self.$$lastDirtyWatch === watcher) {
      return false;
    } 
  });
  return dirty; 
};
```

***NaN的情况***

由于`NaN === NaN => false`,这种情况要处理一下

注意`typeof NaN => "number"`


```js
Scope.prototype.$$areEqual = function(newValue, oldValue, valueEq) {
  if (valueEq) {
    return _.isEqual(newValue, oldValue); 
  } else {
    // 都是NaN的情况返回false
    return newValue === oldValue ||
    // 都是NaN的情况返回true
    (typeof newValue === 'number' && typeof oldValue === 'number' &&
           isNaN(newValue) && isNaN(oldValue));
  }
};
```