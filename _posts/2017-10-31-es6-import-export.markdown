---
layout: post
title:  "es6 import export"
date:   2016-10-31
categories: js
---

http://stackoverflow.com/questions/36795819/when-should-i-use-curly-braces-for-es6-import/36796281#36796281

一个文件中只能有一个`export default`，如`export default 42`
引入的时候不需要大括号，而且名字无所谓

```js
// B.js
import A from './A'
import MyA from './A'
import Something from './A'
```

一个文件可以有很多个named export `export const A = 42`
引入的时候需要大括号，名字要明确,

```js
// B.js
import { A } from './A'
import { myA } from './A' // Doesn't work!
import { Something } from './A' // Doesn't work!
```

当然一个模块default 和其他 named export都有时，可以都引进来

```js
// B.js
import A, { myA, Something } from './A'
```

当然名字也都是可以后天改的。。。

```js
// B.js
import X, { myA as myX, Something as XSomething } from './A'
```

