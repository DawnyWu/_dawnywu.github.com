---
layout: post
title:  "Slide Tags Component"
date:   2016-09-19
categories: React
---

### absolute 绝对定位

记住一个`“positioned”`元素是指`position`值不是`static`的元素。

`absolute`与`fixed` 的表现类似，除了它不是相对于视窗而是相对于最近的`“positioned”`祖先元素

如果绝对定位（position属性的值为absolute）的元素没有“positioned”祖先元素，那么它是相对于文档的 body 元素，并且它会随着页面滚动而移动。


### offsetTop是什么？

the distance of the current element relative to the top of the `offsetParent` node.

### offsetParent是什么？

reference to the object which is the closest (nearest in the containment hierarchy) `positioned` containing element.

If the element is `non-positioned`, the nearest table cell or root element (html in standards compliant mode; body in quirks rendering mode) 

### quirks rendering mode是什么？

* In quirks mode, layout emulates nonstandard behavior in Navigator 4 and Internet Explorer 5. This is essential in order to support websites that were built before the widespread adoption of web standards. 
* In full standards mode, the behavior is (hopefully) the behavior described by the HTML and CSS specifications. 
* In almost standards mode, there are only a very small number of quirks implemented.

***浏览器怎么知道用哪种mode呢？***

根据doctype

开头是这样的`<!DOCTYPE html>`是`full standards mode`

In HTML5, the only purpose of the DOCTYPE is to activate full standards mode.