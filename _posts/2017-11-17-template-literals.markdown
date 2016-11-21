---
layout: post
title:  "template literals"
date:   2016-11-17
categories: js
---

```js
var parts = '/2015/10/Page.html'.match(XRegExp(
  '^ # match at start of string only \n' +
  '/ (?<year> [^/]+ ) # capture top dir name as year \n' +
  '/ (?<month> [^/]+ ) # capture subdir name as month \n' +
  '/ (?<title> [^/]+ ) # capture base name as title \n' +
  '\\.html? $ # .htm or .html file ext at end of path ', 'x'
));
```

不想用加号的话，可以用 `\` 

```js
var parts = '/2015/10/Page.html'.match(XRegExp(
  '^ # match at start of string only \n\
  / (?<year> [^/]+ ) # capture top dir name as year \n\
  / (?<month> [^/]+ ) # capture subdir name as month \n\
  / (?<title> [^/]+ ) # capture base name as title \n\
  \\.html? $ # .htm or .html file ext at end of path ', 'x'
));
```

使用`tagged template`

```js
var parts = '/2015/10/Page.html'.match(XRegExp.rx`
    ^ # match at start of string only
    / (?<year> [^/]+ ) # capture top dir name as year
    / (?<month> [^/]+ ) # capture subdir name as month
    / (?<title> [^/]+ ) # capture base name as title
    \.html? $ # .htm or .html file ext at end of path
`);
```
