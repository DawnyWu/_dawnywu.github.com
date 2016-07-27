---
layout: post
title:  "Build my own angular: Modules And The Injector"
date:   2016-07-20
categories: js
---

`Modules`: Modules are collections of application configuration information.They are where you register your services, controllers, directives, filters, and other application components.

`Injector`: It is the injector that really brings an application to life. 

### module

`module`由`window.angular.module`管理

`angular.module`方法： 输入`module name`, `module's dependencies`,输出`module object`

若`module name`相同会覆盖之前的`module`

```js
// spec
var myModule = window.angular.module('myModule', ['myOtherModule']);
myModule.name = 'myModule'
myModule.requires = ['myOtherModule']
```


```js
var createModule = function(name, requires) {
  var moduleInstance = {
    name: name,
    requires: requires
  };
  return moduleInstance; 
};
```

angular.module不止需要有set功能，还要有get功能

### Injector

```js
// create module
var module = angular.module('myModule', []); 
module.constant('aConstant', 42);

var injector = createInjector(['myModule']); 
expect(injector.has('aConstant')).toBe(true);
```

看似很神奇，根据`module name`创建的`injector`竟然了解`module`的内部

其实是从`angular.module`里面根据`module name`找到的`module`

`module`包含一系列任务，它并不实例化`module`,实例化由`injector`完成,一系列的任务成为`invoke queue.`,每个`module`都有`invoke queue`

```js
invokeQueue=[
              ['constant', ['aConstant', 42]]
            ]

var moduleInstance = {
  name: name,
  requires: requires,
  constant: function(key, value) {
              invokeQueue.push(['constant', [key, value]]);
            },
  _invokeQueue: invokeQueue
};
```

创建`injecotr`的时候把_invokeQueue中的任务都运行一遍就好了,那么`injector`需要知道`_invokeQueue`中不同类型的任务要如何处理，这里我们创建一个`$provide` object

```js
var $provide = {
  constant: function(key, value) {
              cache[key] = value;
            }
};
```

> 感觉好像凡是允许创建object的地方都要检查下，不能允许用户定义`hasOwnProperty`key...

injector实例化module后，对外提供接口

```js
return {
  has: function(key) {return cache.hasOwnProperty(key); },
  get: function(key) { return cache[key]; }
};
```

#### module 依赖的时候

```js
// spec
it('loads the transitively required modules of a module', function() {
  var module1 = angular.module('myModule', []);
  var module2 = angular.module('myOtherModule', ['myModule']);
  var module3 = angular.module('myThirdModule', ['myOtherModule']); 
  module1.constant('aConstant', 42); 
  module2.constant('anotherConstant', 43); 
  module3.constant('aThirdConstant', 44);

  var injector = createInjector(['myThirdModule']);
  expect(injector.has('aConstant')).toBe(true); 
  expect(injector.has('anotherConstant')).toBe(true); 
  expect(injector.has('aThirdConstant')).toBe(true);
});
```

```js
_.forEach(modulesToLoad, function loadModule(moduleName) {
  var module = angular.module(moduleName); 
  // 递归的load
  _.forEach(module.requires, loadModule); 
  _.forEach(module._invokeQueue, function(invokeArgs) {
    var method = invokeArgs[0];
    var args = invokeArgs[1]; 
    $provide[method].apply($provide, args);
  }); 
});
```

***circular dependency的问题***

```js
it('loads each module only once', function() { 
  angular.module('myModule', ['myOtherModule']); 
  angular.module('myOtherModule', ['myModule']);

  createInjector(['myModule']);
});
```

为解决这个问题，引入`var loadedModules = {};` object, 记录加载了哪些`module`

```js
_.forEach(modulesToLoad, function loadModule(moduleName) { 
  if (!loadedModules.hasOwnProperty(moduleName)) {
    loadedModules[moduleName] = true;
    ...
```

### Dependency Injection

```js
it('invokes an annotated function with dependency injection', function() {
  var module = angular.module('myModule', []);
  module.constant('a', 1);
  module.constant('b', 2);
  var injector = createInjector(['myModule']);

  var fn = function(one, two) { return one + two; };
  // 表示fn需要a,b变量, injector invoke fn的时候会在cache中找
  fn.$inject = ['a', 'b'];

  expect(injector.invoke(fn)).toBe(3);
});

function invoke(fn) {
  var args = _.map(fn.$inject, function(token) {
    return cache[token]; 
  });
  return fn.apply(null, args); 
}
```

当函数是object value时，this需要注意处理

```js
it('invokes a function with the given this context', function() { 
  var module = angular.module('myModule', []); 
  module.constant('a', 1);
  var injector = createInjector(['myModule']);

  var obj = { two: 2,
               fn: function(one) { return one + this.two; } };

  obj.fn.$inject = ['a'];
  expect(injector.invoke(obj.fn, obj)).toBe(3);
});
```

```js
function invoke(fn, self) {
  var args = _.map(fn.$inject, function(token) {
    if (_.isString(token)) { 
      return cache[token];
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token;
    }
  });
  // 之前是null
  return fn.apply(self, args); 
}
```

有的时候我们需要覆盖掉为fn提供的变量

```js
it('overrides dependencies with locals when invoking', function() {
  var module = angular.module('myModule', []);
  module.constant('a', 1);
  module.constant('b', 2);
  var injector = createInjector(['myModule']);

  var fn = function(one, two) { return one + two; };
  fn.$inject = ['a', 'b'];
  // 我们提供b覆盖modle的b
  expect(injector.invoke(fn, undefined, {b: 3})).toBe(4);
});
```

```js
// 添加了locals变量
function invoke(fn, self, locals) {
  var args = _.map(fn.$inject, function(token) {
    if (_.isString(token)) {
      // locals的变量优先级高
      return locals && locals.hasOwnProperty(token) ?
              locals[token] :
              cache[token]; 
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token; 
    }
  });
  return fn.apply(self, args); 
}
```

#### Array-Style Dependency Annotation

```js
// spec
var injector = createInjector([]); 
var fn = function() { };
fn.$inject = ['a', 'b'];
expect(injector.annotate(fn)).toEqual(['a', 'b']);

// or....

var injector = createInjector([]);
var fn = ['a', 'b', function() { }]; 
expect(injector.annotate(fn)).toEqual(['a', 'b']);
```

```js
function annotate(fn) { 
  if (_.isArray(fn)) {
    return fn.slice(0, fn.length - 1); 
  } else if (fn.$inject) {
    return fn.$inject; 
  } else {
    return []; 
  }
}
```

有的时候fn没有提供参数。。。那么我们只能去fn的里边去找，从定义fn的表达式去找

```js
var injector = createInjector([]);
var fn = function(a, b) { }; 
expect(injector.annotate(fn)).toEqual(['a', 'b']);
```

使用正则表达式

```js
// match arguments
var FN_ARGS = /^function\s*[^\(]*\(\s*([^\)]*)\)/m;

// match any heading and trailing whitespace in a string
var FN_ARG = /^\s*(\S+)\s*$/;

function annotate(fn) { 
  if (_.isArray(fn)) {
    return fn.slice(0, fn.length - 1); 
  } else if (fn.$inject) {
    return fn.$inject;
  } else if (!fn.length) {
    return []; 
  } else {
    var argDeclaration = fn.toString().match(FN_ARGS);
    return _.map(argDeclaration[1].split(','), function(argName) {
      return argName.match(FN_ARG)[1]; 
    });
  } 
}
```

此处省略一大段处理代码中有comment的内容

还要处理函数参数有下划线的情况,此处省略。。。

```js
var injector = createInjector([]);
var fn = function(a, _b_, c_, _d, an_argument) { }; 
expect(injector.annotate(fn)).toEqual(['a', 'b', 'c_', '_d', 'an_argument']);
```

#### Integrating Annotation with Invocation

We are now able to extract dependency names using the three different methods that Angular supports: `$inject`, `array wrapper`, and `function source extraction`.

```js
// spec
var fn = ['a', 'b', function(one, two) { return one + two; }];
expect(injector.invoke(fn)).toBe(3);

var fn = function(a, b) { return a + b; };
expect(injector.invoke(fn)).toBe(3);
```

#### Instantiating Objects with Dependency Injection

```js
it('instantiates an annotated constructor function', function() { 
  var module = angular.module('myModule', []); 
  module.constant('a', 1);
  module.constant('b', 2);
  var injector = createInjector(['myModule']);

  function Type(one, two) { 
    this.result = one + two;
  }

  Type.$inject = ['a', 'b'];
  var instance = injector.instantiate(Type);
  expect(instance.result).toBe(3);
  // or
  var instance = injector.instantiate(['a', 'b', Type]);
  expect(instance.result).toBe(3);
  // or  和上边的annotate一样，可以自动从函数定义抽取所需变量
  var instance = injector.instantiate(Type);
  expect(instance.result).toBe(3);
});
```

简单实现的instantiate函数

```js
function instantiate(Type) { 
  var instance = {}; 
  // invoke在上边实现过，相当于Type.apply(null, instance)
  invoke(Type, instance); 
  return instance;
}
```

上边的实现测试可以通过，但是与new 构造函数相比，上边的方法还是有区别的

When you construct an object with `new`, you also set up the prototype chain of the object based on the prototype chain of the constructor.We should respect this behavior in `injector.instantiate`.

```js
function instantiate(Type) {
  var UnwrappedType = _.isArray(Type) ? _.last(Type) : Type; 
  var instance = Object.create(UnwrappedType.prototype); 
  invoke(Type, instance);
  return instance;
}
```

******

### invoke函数进化史

`angular`要实现依赖注入，要实现函数可以自动寻找所需参数的功能，比如`function(a,b)`,`angular`可以在运行时找到模块中已有的`a,b`,`invoke`就是负责准备函数所需的参数，然后运行函数

```js
function invoke(fn) {
  var args = _.map(fn.$inject, function(token) {
    if (_.isString(token)) { 
      return cache[token];
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token;
    } 
  });
  return fn.apply(null, args); 
}
```

当spec是下边的时候我们就要注意一下`this`的问题，上边调用函数的语句是`fn.apply(null, args)`，没有this什么事。可是如果fn是一个obj中的value,并且fn中调用了obj中的其他值的话，之前`fn.apply(null, args)`的写法就是有问题的，我们要改写为`fn.apply(this, args)`

```js
it('invokes a function with the given this context', function() { 
  var module = angular.module('myModule', []); 
  module.constant('a', 1);
  var injector = createInjector(['myModule']);
  var obj = { two: 2,
              fn: function(one) { return one + this.two; } };
  obj.fn.$inject = ['a'];
  expect(injector.invoke(obj.fn, obj)).toBe(3);
});
```

```js
function invoke(fn, self) {
  var args = _.map(fn.$inject, function(token) {
    if (_.isString(token)) { 
      return cache[token];
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token;
    } 
  });
  return fn.apply(self, args); 
}
```

我们还需要invoke函数可以接收locals, `injector.invoke(fn, undefined, {b: 3})`,所以准备参数的时候优先从`locals`找，然后再从`cache`中找

```js
function invoke(fn, self, locals) {
  var args = _.map(fn.$inject, function(token) {
    if (_.isString(token)) {
      return locals && locals.hasOwnProperty(token) ?
        locals[token] :
        cache[token]; 
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token; 
    }
  });
  return fn.apply(self, args); 
}
```

目前使用invoke的方式是`fn = function(m,n){}`, `injector.invoke(fn)`, `fn.$inject(a,b)`,写起来比较麻烦，我们希望这种写法 `['a', 'b', function(one, two) { return one + two; }] `

添加`annotate`函数,用来获取函数所需的参数

```js
function annotate(fn) {
  if (_.isArray(fn)) {
    return fn.slice(0, fn.length - 1);
  } else if (fn.$inject) {
    return fn.$inject;
  } else if (!fn.length) {
    return [];
  } else {
    if (strictDi) {
      throw 'fn is not using explicit annotation and cannot be invoked in strict mode';
    }
    var source = fn.toString().replace(STRIP_COMMENTS, '');
    var argDeclaration = source.match(FN_ARGS);
    return _.map(argDeclaration[1].split(','), function(argName) {
      return argName.match(FN_ARG)[2];
    });
  }
}
```

```js
function invoke(fn, self, locals) {
  var args = _.map(annotate(fn), function(token) {
    if (_.isString(token)) {
      return locals && locals.hasOwnProperty(token) ?
        locals[token] :
        getService(token);
    } else {
      throw 'Incorrect injection token! Expected a string, got '+token;
    }
  });
  if (_.isArray(fn)) {
    fn = _.last(fn);
  }
  return fn.apply(self, args);
}
```

*****

### instantiate函数的进化史

之前讲的都是注入`plain function`,其实也是可以注入`构造函数`的,使用`injector.instantiate`

```js
it('instantiates an annotated constructor function', function() { 
  var module = angular.module('myModule', []); 
  module.constant('a', 1);
  module.constant('b', 2);
  var injector = createInjector(['myModule']);

  function Type(one, two) { 
    this.result = one + two;
  }
  Type.$inject = ['a', 'b'];

  var instance = injector.instantiate(Type);
  expect(instance.result).toBe(3);
});
```

之前为了方便书写，`invoke`可以接受`array`形式的参数，`instantiate`也是可以的哦,形如`injector.instantiate(['a', 'b', Type])`。也可以根据构造函数定义自动提取所需变量，利用正则表达式，`annotate`函数中有细节

其实总起上感觉和上边invoke的使用方法是类似的

先创建一个空对象，然后作为this运行构造函数

```js
function instantiate(Type) {
  var instance = {}; 
  invoke(Type, instance); 
  return instance;
}
```
















