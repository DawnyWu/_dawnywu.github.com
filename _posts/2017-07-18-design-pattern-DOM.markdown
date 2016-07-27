---
layout: post
title:  "Design Patterns-DOM"
date:   2016-07-18
categories: js
---

### capability detection.

```js
// antipattern
if (navigator.userAgent.indexOf('MSIE') !== −1) {
document.attachEvent('onclick', console.log); }
// better
if (document.attachEvent) {
document.attachEvent('onclick', console.log); }
// or even more specific
if (typeof document.attachEvent !== "undefined") {
document.attachEvent('onclick', console.log); }
```

### manipulate dom element

***添加的时候***

使用`createDocumentFragment()`

```js
var p, t, frag;
frag = document.createDocumentFragment();
p = document.createElement('p');
t = document.createTextNode('first paragraph'); p.appendChild(t);
frag.appendChild(p);
p = document.createElement('p');
t = document.createTextNode('second paragraph'); p.appendChild(t);
frag.appendChild(p);
document.body.appendChild(frag);
// In this example the live document is updated only once,
// causing a single reflow/repaint,
```

***更新的时候***

使用`cloneNode`

```js
var oldnode = document.getElementById('result'),
clone = oldnode.cloneNode(true);
// work with the clone...
// when you're done: 
oldnode.parentNode.replaceChild(clone, oldnode);
```