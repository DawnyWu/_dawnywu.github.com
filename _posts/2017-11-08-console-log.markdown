---
layout: post
title:  "JS console log"
date:   2016-11-08
categories: js
---

### pretty JSON.stringify()

```js
JSON.stringify(data, null, 2)
```

### pretty Array

```js
var languages = [
    { name: "JavaScript", fileExtension: ".js" },
    { name: "TypeScript", fileExtension: ".ts" },
    { name: "CoffeeScript", fileExtension: ".coffee" }
];

console.table(languages);
```

可以选择列

```js
console.table(languages, "name");
console.table(languages, ["name", "paradigm"]);
```

