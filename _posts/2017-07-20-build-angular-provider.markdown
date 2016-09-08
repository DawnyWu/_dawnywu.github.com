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


#### Circular Dependencies



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

### 思想进化史

原来`module`只记录`constant`task,现在有`provider`task了

```js
var moduleInstance = {
  name: name,
  requires: requires,
  // module.constant('a', 2)
  constant: function(key, value) {
    invokeQueue.push(['constant', [key, value]]);
  },
  // module.provider('b', { $get: function() {return 42} })
  provider: function(key, provider) { 
    invokeQueue.push(['provider', [key, provider]]);
  },
  _invokeQueue: invokeQueue
};
```

现在invokeQueue的样子大概是

```js
[
  ['constant', ['a', 2]],
  ['constant', ['a', 2]],
  ['constant', ['a', 2]],
  ['provider', ['b', { $get: function() {return 42} }]],
  ['provider', ['b', { $get: function() {return 42} }]],
  ['provider', ['b', { $get: function() {return 42} }]],
] 
```

抽象出`invokeLater`函数,有时候会奇怪`arguments`是什么？是module.constant(arguments), module.provider(arguments)

```js
//var moduleInstance = {
//  name: name,
//  requires: requires,
//  constant: invokeLater('constant'), 
//  provider: invokeLater('provider'), 
//  _invokeQueue: invokeQueue
//};

var invokeLater = function(method) { 
  return function() {
    invokeQueue.push([method, arguments]);
    return moduleInstance; 
  };
};
```

$provide object中存有如何运行task的代码

```js
var $provide = {
  constant: function(key, value) {
    if (key === 'hasOwnProperty') {
      throw 'hasOwnProperty is not a valid constant name!';
    }
    cache[key] = value;
  },
  provider: function(key, provider) {
    cache[key] = provider.$get();
  } 
};
```

上边的代码看似够用了，但是当`$get`函数需要依赖注入的时候怎么办。。。

```js
module.provider('b', {
  $get: function(a) { return a + 2; }
});
```

在运行`provider.$get()`时要找到所需的参数`a`,我们好像有之前的`invoke`函数可以用哦,于是修改为

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

`invoke`调用函数的时候会在`cache`中找函数所需的变量并注入,第二个参数`provider`的原因是设定`this`,以免`$get函数`调用`provider obj`中的其他value时不能找到

虽然`injector`和`provider`在书里是分两个章节写的，可在代码中是一部分，没有`injector`的`invoke`等功能，`provider`是不能完成依赖注入功能的

现在要实现lazy的功能了,也就是provider只有在使用的时候运行,使用之前一直都是个没有运行的函数,我们需要在创建另一个`providerCache`来存`provider`。为了做区别，provider没有运行前名字后边加个`Provider`后缀 

`$provide object`变成了这样

```js
// old:
// var $provide = {
//   constant: function(key, value) {
//     if (key === 'hasOwnProperty') {
//       throw 'hasOwnProperty is not a valid constant name!';
//     }
//     cache[key] = value;
//   },
//   provider: function(key, provider) {
//     cache[key] = invoke(provider.$get, provider);
//   } 
// };
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
```

如上代码所示，分别存入不同的`Cache`,`provider:`中不再执行`invoke`，因为要`lazy`,加个后缀保存一下就可以了

我们知道`invoke`的原理就是在`cache`中找所需的参数，那么现在`cache`变成两个了，我们创建一个`getService`函数来做这件事情,其实我觉得倒不如叫`getFromCache`什么的。。。

```js
function getService(name) {
  if (instanceCache.hasOwnProperty(name)) {
    return instanceCache[name];
  } else if (providerCache.hasOwnProperty(name + 'Provider')) {
    var provider = providerCache[name + 'Provider'];
    return invoke(provider.$get, provider); 
  }
}
```

然后`invoke`函数就改写为

```js
function invoke(fn, self, locals) {
  var args = _.map(annotate(fn), function(token) {
    if (_.isString(token)) {
      return locals && locals.hasOwnProperty(token) ?
              locals[token] :
              // 从cache中取用这句了
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

`injector`对外的`has`和`get`无非是`cache`对外的`get`接口而已,所以也要改一下喽

```js
return {
  has: function(key) {
    // 要查两个cache
    return instanceCache.hasOwnProperty(key) || 
    providerCache.hasOwnProperty(key + 'Provider');
  },
  get: getService,
  annotate: annotate,
  invoke: invoke
};
```

现在要实现`provider`的`singleton`, 也就是`provider`只运行一遍

只需要运行后把它存入`instance cache`就好了,原来在`providerCache`中是`XXProvider`,现在存入instanceCache就把Provider几个字去掉了

```js
function getService(name) {
  if (instanceCache.hasOwnProperty(name)) {
    return instanceCache[name];
  } else if (providerCache.hasOwnProperty(name + 'Provider')) {
    var provider = providerCache[name + 'Provider'];
    var instance = instanceCache[name] = invoke(provider.$get); 
    return instance;
  } 
}
```

后来我们要支持`Provider Constructors`, 原来我们的provider是一个有`$get`方法的对象，现在是一个构造函数，它实例化出来的对象是有`$get`方法的。

我们之前说过provider归根到底是一个有`$get`方法的对象而已，我们要把`Provider Constructors`实例化,然后把对象放到`providerCache`里就好了

```js
provider: function(key, provider) {
  // 实例化provider constructor,然后存入providerCache
  if (_.isFunction(provider)) {
    provider = instantiate(provider);
  }
  providerCache[key + 'Provider'] = provider;
}
```

~~那么现在`providerCache`里有两种东西~~

只有一种东西而已,就是一个有`$get`方法的对象, 这时候并没有添加新的依赖注入问题，之前的注入代码不需要改动的

那构造函数这种方式提供`provider`有什么特殊的地方呢？
它可以注入另外一个constructor provider, 这个是怎么实现的呢？

```js
module.provider('a', function AProvider() { 
  var value = 1;
  this.setValue = function(v) { value = v; }; 
  this.$get = function() { return value; };
});

module.provider('b', function BProvider(aProvider) {
  aProvider.setValue(2);
  this.$get = function() { }; 
});

// aProvider obj
{
  setValue: fun,
  $get: fun,
}
```


实例化的时候运行`instantiate`,代码如下，运行invoke的时候会去`cache`中查找`aProvider`,`aProvider`是一个object,不是构造函数哦。这个查找功能是代理给`getService`函数的,因为参数名字就叫XXProvider,所以直接去providerCache中找就可以了,原来的参数都是类似a，然后存入cache前变的名子

```js
// 新加这句
} else if (providerCache.hasOwnProperty(name)) {
  return providerCache[name];
} else if (providerCache.hasOwnProperty(name + 'Provider')) {
```




```js
function instantiate(Type, locals) {
  var UnwrappedType = _.isArray(Type) ? _.last(Type) : Type;
  // 实例化Type类,可是locals还没有用上
  var instance = Object.create(UnwrappedType.prototype); 
  // invoke(fn, self, locals)
  invoke(Type, instance, locals);
  return instance;
}
```

以前我们注入的是constant,或者$get函数的返回值，现在我们可以注入一个provider了有木有。。。。

providerCache现在又用来提供provider,又用来执行$get函数提供给isntanceCache

由于一大堆spec....此处省略，我们需要两种injector, 一种The injection that happens between provider constructors only deals with other providers.
一种The injection that happens between $get methods and the external injector API only deals with instances. 












