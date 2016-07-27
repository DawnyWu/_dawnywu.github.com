---
layout: post
title:  "Build my own angular: High-Level Dependency Injection Features"
date:   2016-07-21
categories: js
---

### High-Level Dependency Injection Features

#### Injecting The $injectors

```js
it('allows injecting the instance injector to $get', function() {
  var module = angular.module('myModule', []);
  module.constant('a', 42);
  module.provider('b', function BProvider(){
    // 这里可以传入injector
    this.$get = function($injector) {
      return $injector.get('a');
    }
  })

  var injector = createInjector(['myModule'])

  expect(injector.get('b')).toBe(42)
})
```

```js
var instanceInjector = instanceCache.$injector =
createInternalInjector(instanceCache, function(name) {
  var provider = providerInjector.get(name + 'Provider');
  return instanceInjector.invoke(provider.$get, provider); 
});
```

Similarly, you can inject $injector to a provider constructor.`Provider constructors` only have `other providers` and `constants` available for injection. 

```js
it('allows injecting the provider injector to provider', function(){
  var module = angular.module('myModule', []);

  module.provider('a', function AProvider() {
    this.value = 42;
    this.$get = function() { return this.value; };
  });

  // 这里可以传入$injector
  module.provider('b', function BProvider($injector) {
    var aProvider = $injector.get('aProvider');
    this.$get = function() {
      return aProvider.value; 
    };
  });

  var injector = createInjector(['myModule']);

  expect(injector.get('b').toBe(42);
})
```

```js
var providerInjector = providerCache.$injector = 
createInternalInjector(providerCache, function() {
  throw 'Unknown provider: '+path.join('<-');
})
```

#### Injecting $provide

`$inject`是一个只读的api,我们要是想要写的话，要使用`$provide`

By injecting `$provide` you gain direct access to the methods we’ve been calling through the module invoke queue. 

```js
it('allows injecting the $provide service to providers', function() { 
  var module = angular.module('myModule', []);
  module.provider('a', function AProvider($provide) {
    $provide.constant('b', 2);
    this.$get = function(b) { return 1 + b; };
  });

  var injector = createInjector(['myModule']);

  expect(injector.get('a')).toBe(3);
});
```

Crucially though, `$provide` is only available through the provider injector

下面这种是不允许的

```js
it('does not allow injecting the $provide service to $get', function() { 
  var module = angular.module('myModule', []);
  module.provider('a', function AProvider() {
    this.$get = function($provide) { };
  });
  var injector = createInjector(['myModule']);
  expect(function() { injector.get('a');
   }).toThrow();
});
```

`$provide` object you inject is in fact the `$provide` object we already have - the one with the `constant` and `provider` methods.

#### Config Blocks

a `config block` is an arbitrary function that has its dependencies injected from the provider cache. 

#### Run Blocks

```js
it('runs run blocks when the injector is created', function(){ 
  var module = angular.module('myModule', []);
  var hasRun = false; 
  module.run(function() { hasRun = true; });

  createInjector(['myModule']);
  expect(hasRun).toBe(true);
});
```

The main difference  between `config blocks` and `run blocks` is that run blocks are injected from the instance cache

```js
it('injects run blocks with the instance injector', function() { 
  var module = angular.module('myModule', []);

  module.provider('a', {$get: _.constant(42)});

  var gotA; 

  module.run(function(a) {gotA = a; });

  createInjector(['myModule']);
  expect(gotA).toBe(42);
});
```

The purpose of run blocks is not to configure providers - you can’t even inject them here - but to just run some arbitrary code you want to hook on to the Angular startup process.

#### Function Modules

There is also an alternative way you can define a module: A module can be just a function, which will be injected from the provider injector when loaded.

```js
it('runs a function module dependency as a config block', function() { 
  var functionModule = function($provide) {
    $provide.constant('a', 42);
  };
  angular.module('myModule', [functionModule]);
  var injector = createInjector(['myModule'])
  expect(injector.get('a')).toBe(42)
})
```


