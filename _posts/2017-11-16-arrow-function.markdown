---
layout: post
title:  "arrow function"
date:   2016-11-16
categories: js
---


### arrow function IIFE

```js
(function () { // open IIFE
    // inside IIFE
}()); // close IIFE
```

=

```js
(
  function () { // open IIFE
      // inside IIFE
  }()
); // close IIFE
```

arrow function版是这样, 注意小括号加在了哪里

```js
(() => {
    return 123
})();
```

=

```js
(
  () => {
      return 123
  }
)();
```