---
layout: post
title:  "Build my own angular: Scope Inherit"
date:   2016-07-21
categories: js
---

### The Root Scope

```js
var scope = new Scope();
```

A scope created like this is a root scope.

可是实际上我们不会创建这种scope,我们要使用angular提供的`$rootScope`

#### Making A Child Scope

```js
it("inherits the parent's properties", function() { 
  var parent = new Scope();
  parent.aValue = [1, 2, 3];
  var child = parent.$new(); 
  expect(child.aValue).toEqual([1, 2, 3]);
});
```

```js
it("can manipulate a parent scope's property", function() { 
  var parent = new Scope();
  var child = parent.$new();
  parent.aValue = [1, 2, 3];

  child.aValue.push(4);
  expect(child.aValue).toEqual([1, 2, 3, 4]);
  expect(parent.aValue).toEqual([1, 2, 3, 4]);
});
```

```js
it("can watch a property in the parent", function() { 
  var parent = new Scope();
  var child = parent.$new();
  parent.aValue = [1, 2, 3];
  child.counter = 0;
  child.$watch(
    function(scope) { return scope.aValue; }, 
    function(newValue, oldValue, scope) {
        scope.counter++;
    },
    true
  );
  child.$digest();
  expect(child.counter).toBe(1);
  parent.aValue.push(4);
  child.$digest();
  expect(child.counter).toBe(2);
});
```

实现：


```js
Scope.prototype.$new = function() { 
  var ChildScope = function() { }; 
  ChildScope.prototype = this;
  var child = new ChildScope(); 
  return child;
};
```

#### Attribute Shadowing

```js
it("shadows a parent's property with the same name", function() { 
  var parent = new Scope();
  var child = parent.$new(); 
  parent.name = 'Joe';
  child.name ='Jill'
  expect(child.name).toBe('Jill');
  expect(parent.name).toBe('Joe');
});
```

上边是Attribute Shadowing,没什么，看下边这个

```js
it("does not shadow members of parent scope's attributes", function() {
  var parent = new Scope();
  var child = parent.$new(); 
  parent.user = {name: 'Joe'};
  child.user.name = 'Jill';

  expect(child.user.name).toBe('Jill')
  expect(parent.user.name).toBe('Jill')
})
```

The reason this works is that we don’t assign anything on the child scope. We merely read the user attribute from the scope and assign something within that object. Both scopes have a reference to the same user object, which is a plain JavaScript object that has nothing to do with scope inheritance.

#### Separated Watches

我们希望`child.$digest()`，只运行`child`的`watch fn`,而不是`parent`的

```js
it("does not digest its parent(s)", function() { 
  var parent = new Scope();
  var child = parent.$new();

  parent.aValue = 'abc';
  parent.$watch(
    function(scope) { return scope.aValue; }, 
    function(newValue, oldValue, scope) {
      scope.aValueWas = newValue;
    }
  );

  child.$digest();
  expect(child.aValueWas).toBeUndefined();
});
```

```js
Scope.prototype.$new = function() { 
  var ChildScope = function() { }; 
  ChildScope.prototype = this;
  var child = new ChildScope(); 
  // 加一句这个就好了
  child.$$watchers = [];
  return child; 
};
```

#### Recursive Digestion

Calling `$digest` should not run watches up the hierarchy.

It should, however, run watches down the hierarchy, on the children of the scope we’re calling.

解决办法是`scope`添加`$$children array`,保存下自己创建的children,实现略。。。


```js
it("digests its children", function() { 
  var parent = new Scope();
  var child = parent.$new();

  parent.aValue = 'abc'
  child.$watch(
    function(scope) { return scope.aValue; }, 
    function(newValue, oldValue, scope) {
      scope.aValueWas = newValue;
    }
  );
  parent.$digest();
  expect(child.aValueWas).toBe('abc');
});
```


