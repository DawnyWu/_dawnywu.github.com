---
layout: post
title:  "Build my own angular: Provider"
date:   2016-07-20
categories: js
---

Providers are objects that know how to make dependencies.

what Angular calls a `provider` is any JavaScript object that has a method attached to it called `$get`. When you provide such an object to the injector, it will call that `$get` method and treat its return value as the actual dependency value

```js
it('allows registering a provider and uses its $get', function() { 
  var module = angular.module('myModule', []); 
  module.provider('a', {
    $get: function() { return 42; }
  });

  var injector = createInjector(['myModule']);
  expect(injector.has('a')).toBe(true);
  expect(injector.get('a')).toBe(42);
});
```

provider就像之前的constant一样，只是一种任务，留着等待injector来做

```js
var moduleInstance = {
  name: name,
  requires: requires,
  constant: function(key, value) {
              invokeQueue.push(['constant', [key, value]]);
            },
  provider: function(key, provider) { 
              invokeQueue.push(['provider', [key, provider]]);
            },
  _invokeQueue: invokeQueue
};

// 重构constant和provider的代码
var invokeLater = function(method) { 
  return function() {
           invokeQueue.push([method, arguments]);
           return moduleInstance; 
         };
};
```

在injector要有相对应的函数

```js
var $provide = {
  constant: function(key, value) {
    if (key === 'hasOwnProperty') {
      throw 'hasOwnProperty is not a valid constant name!';
    }
    cache[key] = value;
  },
  // 运行get函数即可
  provider: function(key, provider) {
    cache[key] = provider.$get();
  } 
};
```

#### Injecting Dependencies To The $get Method

`provider`的`get`函数调用`module`的`constant`的情况

```js
it('injects the $get method of a provider', function() {
  var module = angular.module('myModule', []); 
  module.constant('a', 1);
  module.provider('b', {
    $get: function(a) { return a + 2; }
  });
  var injector = createInjector(['myModule']); 
  expect(injector.get('b')).toBe(3);
});
```

```js
var $provide = {
  constant: function(key, value) {
    if (key === 'hasOwnProperty') {
      throw 'hasOwnProperty is not a valid constant name!';
    }
    cache[key] = value;
  },
  provider: function(key, provider) {
    cache[key] = invoke(provider.$get, provider);
  } 
};
```

#### Lazy Instantiation of Dependencies

下边这种情况会失败，实例化b的时候a并不存在。那么我们写依赖的时候就需要注意顺序，怎样可以不需要注意顺序呢？

```js
it('injects the $get method of a provider lazily', function() { 
  var module = angular.module('myModule', []); 
  module.provider('b', {
    $get: function(a) { return a + 2; } 
  });
  module.provider('a', {$get: _.constant(1)});
  var injector = createInjector(['myModule']);
  expect(injector.get('b')).toBe(3);
});
```

所以对provider使用Lazy Instantiation,直到使用provider提供的值的时候再运行get函数

```js
function createInjector(modulesToLoad, strictDi) { 
  // provider存在这里
  var providerCache = {};
  // 原来的cache object变为instanceCache
  var instanceCache = {};
  var loadedModules = {};
  // ...
}

// injector对应的处理方式
var $provide = {
  constant: function(key, value) {
    if (key === 'hasOwnProperty') {
      throw 'hasOwnProperty is not a valid constant name!';
    }
    instanceCache[key] = value;
  },
  provider: function(key, provider) { 
    providerCache[key + 'Provider'] = provider;
  } 
};

// name是另一个构造函数，aProvider的情况
function getService(name) {
  if (instanceCache.hasOwnProperty(name)) {
    return instanceCache[name];
  } else if (providerCache.hasOwnProperty(name + 'Provider')) {
    var provider = providerCache[name + 'Provider'];
    return invoke(provider.$get, provider); 
  }
}

function invoke(fn, self, locals) {
  var args = _.map(annotate(fn), function(token) {
    if (_.isString(token)) {
      return locals && locals.hasOwnProperty(token) ?
        locals[token] 
        // 注意这句
        : getService(token); 
    } else {
        throw 'Incorrect injection token! Expected a string, got ' + token;
    }
  });

  if (_.isArray(fn)){
    fn = _.last(fn);
  }

  return fn.apply(self, args); 
}

```

So, a dependency from a provider only gets instantiated when its either injected somewhere, or explicitly asked for through `injector.get`. If no one ever asks for a dependency, it will never actually come to be - its provider’s `$get` never gets called.

You can check for the existence of a dependency through `injector.has`, which does not cause the dependency to be instantiated. It just checks if there’s either a dependency instance or a provider for it available.
#### Making Sure Everything Is A Singleton

```js
// spec
it('instantiates a dependency only once', function() {
  var module = angular.module('myModule', []); 
  module.provider('a', {$get: function() { return {}; }});
  var injector = createInjector(['myModule']); 
  expect(injector.get('a')).toBe(injector.get('a'));
});
```

目前a是不同的两个对象，因为我们会运行两次get方法

解决办法是运行完`providerCache`后,把值放入`instanceCache`

```js
function getService(name) {
  if (instanceCache.hasOwnProperty(name)) {
    return instanceCache[name];
  } else if (providerCache.hasOwnProperty(name + 'Provider')) {
    var provider = providerCache[name + 'Provider'];
    // invoke后放入instanceCache
    var instance = instanceCache[name] = invoke(provider.$get); 
    return instance;
  } 
}
```

provider环形依赖问题略过

#### Provider Constructors

```js
provider: function(key, provider) { 
  if (_.isFunction(provider)) {
    provider = instantiate(provider);
  }
  providerCache[key + 'Provider'] = provider;
}
```

```js
// spec
var module = angular.module('myModule', []);

module.provider('a', function AProvider() { 
  var value = 1;
  this.setValue = function(v) { value = v; }; 
  this.$get = function() { return value; };
});

module.provider('b',  function BProvider(aProvider) {
  aProvider.setValue(2);
  this.$get = function() { }; 
});

var injector = createInjector(['myModule'])

expect(injector.get('a')).toBe(2)
```

#### Two Injectors: The Provider Injector and The Instance Injector

这两种injector并不是可以互相替代的，他们的使用有着不同的目的

```js
var module = angular.module('myModule',, []);
module.provider('a', function AProvider() { 
  var value = 1;
  this.setValue = function(v) { value = v; }; 
  this.$get = function() { return value; };
});

module.provider('b', function BProvider(aProvider) {
  // 在BProvider中注入了aProvider，并调用其方法
  aProvider.setValue(2);
  this.$get = function() { }; 
});
var injector = createInjector(['myModule'])

expect(injector.get('a')).toBe(2)
```

当然注入也是有限制的,Turns out you can’t just inject either providers or instances anywhere you please.如下所示：

```js
it('does not inject an instance to a provider constructor function', function() { 
  var module = angular.module('myModule',[]);
  module.provider('a', function AProvider() { 
    this.$get = function() { return 1; };
  });
  // 注入a,即AProvider的instance
  module.provider('b', function BProvider(a) { 
    this.$get = function() { return a; };
  });
  expect(function() { 
    createInjector(['myModule']);
  }).toThrow();
})
```

```js
it('does not inject a provider to a $get function',function() { 
  var module = angular.module('myModule', []);
  module.provider('a', function AProvider() { 
    this.$get = function() { return 1; };
  });
  module.provider('b',  function BProvider() {
    // aProvider在b的$get方法里
    this.$get = function(aProvider) { return aProvider.$get(); }; 
  });
  var injector = createInjector(['myModule'])

  expect(function() { 
    injector.get('b');
  }).toThrow();
});
```

```js
it('does not inject a provider to invoke', function() { 
  var module = angular.module('myModule', [])

  module.provider('a',  function AProvider() { 
    this.$get = function() { return 1; }
  });

  var injector = createInjector(['myModule'])
  expect(function() { 
    injector.invoke(function(aProvider) { });
  }).toThrow();
});
```

```js
it('does not give access to providers through get',  function() {
  var module = angular.module('module', [])

  module.provider('a', function AProvider() { 
    this.$get = function() { return 1; };
  });

  var injector = createInjector(['myModule'])
  expect(function() {
    injector.get('aProvider');
  }).toThrow();
});
```

The injection that happens between provider constructors only deals with other providers. 

The injection that happens between $get methods and the external injector API only deals with instances. The instances may be created using providers but the providers are not exposed.

This separation can be implemented by actually having two separate injectors: One that deals exclusively with providers, and another that deals exclusively with instances. 

we’ll have an internal function inside `createInjector` that we’ll use to create our two internal injectors. This function will take two arguments:

* `A cache` to do dependency lookups from
* `a factory function` to fall back to when there’s nothing in the cache:

`function createInternalInjector(cache, factoryFn) { }`, 

```js
function createInjector(modulesToLoad, strictDi) {
  var providerCache = {};
  var providerInjector = createInternalInjector(providerCache, function() {
    throw 'Unknown provider: ' + path.join(' <- ');
  });
  var instanceCache = {};
  var instanceInjector = createInternalInjector(instanceCache, function(name) { 
    var provider = providerInjector.get(name + 'Provider');
    return instanceInjector.invoke(provider.$get, provider); 
  });
  var loadedModules = {}; 
  var path = [];
  strictDi = (strictDi === true);

  var $provide = {
    constant: function(key, value) {
      if (key === 'hasOwnProperty'){
        throw 'hasOwnProperty is not a valid constant name!';
      }
      providerCache[key] = value;
      instanceCache[key] = value;
    },
    provider: function(key, provider) { 
      if (_.isFunction(provider)) {
        provider = providerInjector.instantiate(provider);
      }
      providerCache[key + 'Provider'] = provider;
    }
  };

  function annotate(fn) { 
    if (_.isArray(fn)) {
      return fn.slice(0, fn.length - 1); 
    } else if (fn.$inject) {
      return fn.$inject;
    } else if (!fn.length) {
      return []; 
    } else {
      var source = fn.toString().replace(STRIP_COMMENTS, '')
      var argDeclaration = source.match(FN_ARGS);
      return _.map(argDeclaration[1].split(','), function(argName) {
      return argName.match(FN_ARG)[2]; });
    } 
  }

  function createInternalInjector(cache, factoryFn) {
    function getService(name) {
      if (cache.hasOwnProperty(name)) {
        if (cache[name] === INSTANTIATING) {
          throw new Error('')}
          return cache[name]; 
      } else {
        path.unshift(name); 
        cache[name] = INSTANTIATING; 
        try {
          return (cache[name] = factoryFn(name)); 
        } finally {
          path.shift();
          if (cache[name] === INSTANTIATING) {
          delete cache[name]; 
          }
        } 
      }
    }

  function invoke(fn, self, locals) {
    // fn是个构造函数的话, annotate得出aProvider, 后边getService aProvider
    var args = _.map(annotate(fn), function(token) {
      if (_.isString(token)) {
        return locals && locals.hasOwnProperty(token) ?
          locals[token] :
          getService(token); 
      } else {
        throw ''
      }
    });
    if (_.isArray(fn)) {
      fn = _.last(fn);
    }
    return fn.apply(self, args); 
  }

  // Type是个构造函数
  function instantiate(Type, locals) {
    var UnwrappedType = _.isArray(Type) ? _.last(Type) : Type;
    // 实例化Type类,可是locals还没有用上
    var instance = Object.create(UnwrappedType.prototype); 
    // invoke(fn, self, locals)
    invoke(Type, instance, locals);
    return instance;
  }

return {
has: function(name) {
return cache.hasOwnProperty(name) || providerCache.hasOwnProperty(name +'Provider')},
    get: getService,
    annotate: annotate,
    invoke: invoke,
    instantiate: instantiate
}; }

  _.forEach(modulesToLoad, function loadModule(moduleName) {
    // 避免重复加载, 加载过的module放在loadedModules obj里标明true
    if (!loadedModules.hasOwnProperty(moduleName)) {
      loadedModules[moduleName] = true;

      var module = angular.module(moduleName); 
      // 对于有依赖的module，继续运行loadModule方法,递归
      _.forEach(module.requires, loadModule); 
      // 调用module的_invokeQueue   
      // invokeArgs是['constant', [key, value]]类似格式
      _.forEach(module._invokeQueue, function(invokeArgs) {
        var method = invokeArgs[0];
        var args = invokeArgs[1];
        $provide[method].apply($provide, args);
      }); 
    }
  });

  return instanceInjector; 
}
```





























#### Unshifting Constants in The Invoke Queue

Constants will always be registered first.

so you can have a provider depending on a constant that’s registered later:

```js
it('registers constants first to make them available to providers', function() { 
  var module = angular.module('myModule', []);

  module.provider('a', function AProvider(b) { 
    this.$get = function() { return b; };
  });
  module.constant('b', 42);
  var injector = createInjector(['myModule']);
  expect(injector.get('a')).toBe(42);
 });
```

When constants are registered to a module, the module loader always adds them to the front of the invoke queue.

实现的原理很简单，把其他类型推入invokeQueue的时候用的`push`方法，对`constant`使用`unshift`方法