---
layout: post
title:  "mobile fixed position"
date:   2016-11-03
categories: mobile
---

https://remysharp.com/2012/05/24/issues-with-position-fixed-scrolling-on-ios



```css
.fixed {
  position: fixed;
  top: 0px;
  left: 0px;
  width: 320px;
  height: 50px;
  background: red;
  -webkit-backface-visibility: hidden;
  /*--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Most Important*/
}
```
