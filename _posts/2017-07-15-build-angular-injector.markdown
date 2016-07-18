---
layout: post
title:  "Build my own angular: Injector"
date:   2016-07-15
categories: js
---

#### $provide

```js
it('allows injecting the $provide service to providers', function() {
  var module = angular.module('myModule', []);
  module.provider('a', 
    function AProvider($provide) { 
      $provide.constant('b', 2);
      this.$get = function(b) { return 1 + b; }; 
    }
  );
  var injector = createInjector(['myModule']);
  expect(injector.get('a')).toBe(3);
});
```

```js
it('does not allow injecting the $provide service to $get', function() {
  var module = angular.module('myModule', []);
  module.provider('a',
    function AProvider() { 
      // 在这里不允许
      this.$get = function($provide) { };
    }
  );
  var injector = createInjector(['myModule']);
  expect(function() { 
    injector.get('a');
  }).toThrow();
});
```

#### Config Blocks

```js
it('runs config blocks when the injector is created', function() {
  var module = angular.module('myModule', []);
  var hasRun = false;
  module.config(function() {hasRun = true; });
  createInjector(['myModule']);
  expect(hasRun).toBe(true);
});
```

#### Services

```js
it('allows registering a service', function() {
  var module = angular.module('myModule', []);
  module.service('aService', 
    // Service是一个构造函数
    function MyService() {
      this.getValue = function() { return 42; };
    }
  );
  var injector = createInjector(['myModule']);
  expect(injector.get('aService').getValue()).toBe(42);
});
```