---
layout: post
title:  "Build my own angular: Scope Events"
date:   2016-07-22
categories: js
---

### Scope Events

沿着scope结构，向上发送事件叫`emmitting`,向下叫做`broadcasting`

感觉实现起来就是观察者模式的改进而已

```js
function Scope() { 
  ...
  // 添加这个。。。
  this.$$listeners = {}; 
}
```

```js
Scope.prototype.$on = function(eventName, listener) { 
  var listeners = this.$$listeners[eventName];
  if (!listeners) {
    this.$$listeners[eventName] = listeners = []; 
  }
  listeners.push(listener);
};
```
因为有`property shadowing`的特性，`children`会和`parent`有相同的`$$listener`

希望达到的效果是这样的,每个children有自己的`$$listener`

```js
it("registers different listeners for every scope", function() { 
  var listener1 = function() { };
  var listener2 = function() { };
  var listener3 = function() { };
  scope.$on('someEvent', listener1);
  child.$on('someEvent', listener2);
  isolatedchild.$on('someEvent', listener3);

  expect(scope.$$listeners).toEqual({someEvent: [listener1]});
  expect(child.$$listeners).toEqual({someEvent: [listener2]});
  expect(isolatedChild.$$listeners).toEqual({someEvent: [listener3]});
});
```

```js
Scope.prototype.$new = function(isolated, parent) { 
  var child;
  parent = parent || this;
  if (isolated) {
    child = new Scope();
    child.$root = parent.$root;
    child.$$asyncQueue = parent.$$asyncQueue; child.$$postDigestQueue = parent.$$postDigestQueue; child.$$applyAsyncQueue = this.$$applyAsyncQueue;
  } else {
    var ChildScope = function() { }; 
    ChildScope.prototype = this; 
    child = new ChildScope();
  } 
  parent.$$children.push(child); 
  child.$$watchers = []; 
  // 加这句
  child.$$listeners = {}; 
  child.$$children = []; 
  child.$parent = parent; 
  return child;
};
```

#### The basics of $emit and $broadcast

```js
//Scope.prototype.$broadcast = function(eventName) {
Scope.prototype.$emit = function(eventName) {
  var listeners = this.$$listeners[eventName] || [];
  _.forEach(listeners, function(listener) {
    listener();
  });
};
```

我们可以看到，`$emit`和`$broadcast`有很多共同点,我们把它抽取出来

```js
Scope.prototype.$$fireEventOnScope = function(eventName) { 
  var listeners = this.$$listeners[eventName] || []; 
  _.forEach(listeners, function(listener) {
    listener();
  });
};
```

#### Event Objects

What we should do is pass the listeners an event object.

```js
Scope.prototype.$$fireEventOnScope = function(eventName) { 
  // 构造event obj 传入 listener
  var event = {name: eventName};
  var listeners = this.$$listeners[eventName] || []; 
  _.forEach(listeners, function(listener) {
    listener(event);
  });
};
```

#### Additional Listener Arguments

```js
Scope.prototype.$emit = function(eventName) { 
  var additionalArgs = _.rest(arguments);
  this.$$fireEventOnScope(eventName, additionalArgs); 
};
```

`additionalArgs`是要当做参数传给`listener`的

```js
Scope.prototype.$$fireEventOnScope = function(eventName, additionalArgs) { 
  var event = {name: eventName};
  // 输出: [{name: eventName}, [additionalArgs]]
  var listenerArgs = [event].concat(additionalArgs);
  var listeners = this.$$listeners[eventName] || [];
  _.forEach(listeners, function(listener) { 
    listener.apply(null, listenerArgs);
  }); 
};
```

#### Returning The Event Object

运行`$emit`后返回`event object`

```js
Scope.prototype.$$fireEventOnScope = function(eventName, additionalArgs) { 
  var event = {name: eventName};
  var listenerArgs = [event].concat(additionalArgs);
  var listeners = this.$$listeners[eventName] || [];
  _.forEach(listeners, function(listener) { 
    listener.apply(null, listenerArgs);
  });
  // 加这行
  return event; 
};
```

#### Deregistering Event Listeners

`Deregistering`事件的方式有点特别啊，我感觉。。。。

```js
it("can be deregistered "+method, function() {
  var listener = jasmine.createSpy();
  var deregister = scope.$on('someEvent', listener);
  deregister();
  scope[method]('someEvent');
  expect(listener).not.toHaveBeenCalled();
}); 
```

```js
Scope.prototype.$on = function(eventName, listener) { 
  var listeners = this.$$listeners[eventName];
  if (!listeners) {
    this.$$listeners[eventName] = listeners = [];
  }
  listeners.push(listener); 
  // 返回deregister函数
  return function() {
    var index = listeners.indexOf(listener); 
    if (index >= 0) {
      listeners.splice(index, 1);
    }
  }; 
};
```

```js
it("does not skip the next listener when removed on "+method, function() { 
  var deregister;
  // 
  var listener = function() { 
    // ....
    // 为了只执行一次，调用deregister函数
    deregister(); 
  };
  var nextListener = jasmine.createSpy();

  deregister = scope.$on('someEvent', listener);
  scope.$on('someEvent',  nextListener);
  
  // 触发someEvent事件， 在$$listener数组中遍历，执行listener函数，他删掉了自己。。。后边的一个遍历不到，被跳过
  scope[method]('someEvent');
  expect(nextListener).toHaveBeenCalled();
});
```

解决的办法是不删掉`listener`,而是把它变成`null`

```js
Scope.prototype.$on = function(eventName, listener) { 
  var listeners = this.$$listeners[eventName];
  if (!listeners) {
    this.$$listeners[eventName] = listeners = []; 
  }
  listeners.push(listener); 
  return function() {
    var index = listeners.indexOf(listener);
    if (index >= 0) {
      listeners[index] = null; 
    }
  }; 
};
```

#### Emitting Up The Scope Hierarchy

```js
it("propagates up the scope hierarchy on $emit", function() { 
  var parentListener = jasmine.createSpy();
  var scopeListener = jasmine.createSpy();
  parent.$on('someEvent', parentListener);
  scope.$on('someEvent', scopeListener);

  scope.$emit('someEvent')

  expect(scopeListener).toHaveBeenCalled();
  expect(parentListener).toHaveBeenCalled();
});
```

```js
Scope.prototype.$emit = function(eventName) { 
  var additionalArgs = _.rest(arguments); 
  var scope = this;
  do {
    scope.$$fireEventOnScope(eventName, additionalArgs);
    // 触发后找parent再触发
    scope = scope.$parent; 
  } while (scope);
};
```

我们要求返回的event事件要相同

```js
it("propagates the same event up on $emit", function() { 
  var parentListener = jasmine.createSpy();
  var scopeListener = jasmine.createSpy();
  parent.$on('someEvent',parentListener);
  scope.$on('someEvent',  scopeListener);
  scope.$emit('someEvent')

  var scopeEvent = scopeListener.calls.mostRecent().args[0]; 
  var parentEvent = parentListener.calls.mostRecent().args[0]; 
  expect(scopeEvent).toBe(parentEvent);
})
```

```js
Scope.prototype.$emit = function(eventName) {
  var event = {name: eventName};
  var listenerArgs = [event].concat(_.rest(arguments)); 
  var scope = this;
  do {
    scope.$$fireEventOnScope(eventName, listenerArgs);
    scope = scope.$parent; 
  } while (scope);
  return event;
};

Scope.prototype.$broadcast = function(eventName) {
  var event = {name: eventName};
  var listenerArgs = [event].concat(_.rest(arguments)); 
  this.$$fireEventOnScope(eventName, listenerArgs); 
  return event;
};

Scope.prototype.$$fireEventOnScope = function(eventName, listenerArgs) {
  var listeners = this.$$listeners[eventName] || [];
  var i = 0;
  while (i < listeners.length) {
    if (listeners[i] === null) { 
      listeners.splice(i, 1);
    } else {
      listeners[i].apply(null, listenerArgs);
      i++; 
    }
  } 
};
```

#### Broadcasting Down The Scope Hierarchy

broadcast向下传播，是树形的

```js
Scope.prototype.$broadcast = function(eventName) {
  var event = {name: eventName};
  var listenerArgs = [event].concat(_.rest(arguments)); 
  // 借用chapter2的everyScope函数
  this.$$everyScope(function(scope) {
    scope.$$fireEventOnScope(eventName, listenerArgs);
    return true; 
  });
  return event; 
};
```

#### Including The Current And Target Scopes in The Event Object

类似DOM的`target`和`currentTarget`,scope有`targetScope`和`currentScope`

* `targetScope` identifies the scope on which the event occurred 
* `currentScope` identifies the scope on which the listener was attached. 

```js
Scope.prototype.$emit = function(eventName) {
  // 加这个
  var event = {name: eventName, targetScope: this}; 
  var listenerArgs = [event].concat(_.rest(arguments)); 
  var scope = this;

  do {
    scope.$$fireEventOnScope(eventName, listenerArgs);
    scope = scope.$parent; 
  } while (scope);
  return event;
};
```

```js
Scope.prototype.$emit = function(eventName) {
  var event = {name: eventName, targetScope: this}; 
  var listenerArgs = [event].concat(_.rest(arguments)); 
  var scope = this;
  do {
    // 加这个
    event.currentScope = scope;
    scope.$$fireEventOnScope(eventName, listenerArgs);
    scope = scope.$parent;
  } while (scope);
  return event; 
};
```

```js
Scope.prototype.$broadcast = function(eventName) {
  var event = {name: eventName, targetScope: this}; 
  var listenerArgs = [event].concat(_.rest(arguments)); 
  this.$$everyScope(function(scope) {
    event.currentScope = scope; 
    scope.$$fireEventOnScope(eventName, listenerArgs); 
    return true;
  });
  return event; 
};
```

#### Stopping Event Propagation

只有`emit`可以`stopPropagation`

在当前`scope stopPropagation`，当前`scope`的`handler`还是可以触发的

```js
Scope.prototype.$emit = function(eventName) { 
  // new line
  var propagationStopped = false;
  var event = {
    name: eventName, 
    targetScope: this, 
    // new line
    stopPropagation: function() {
      propagationStopped = true; 
    }
  };
  var listenerArgs = [event].concat(_.rest(arguments)); 
  var scope = this;
  do {
      event.currentScope = scope;
      scope.$$fireEventOnScope(eventName, listenerArgs);
      scope = scope.$parent;
  // new line
  } while (scope && !propagationStopped);
  return event; 
};
```
















