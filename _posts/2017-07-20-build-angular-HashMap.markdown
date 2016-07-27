---
layout: post
title:  "Build my own angular: HashMap"
date:   2016-07-21
categories: js
---

a value’s hash key has two parts to it: 

* The first part designates the type of the value

* The second part designates the value’s string representation.

The two parts are separated with a colon.

```js
//spec
expect(hashKey(undefined)).toEqual('undefined:undefined');
expect(hashKey(null)).toEqual('object:null');
expect(hashKey(true)).toEqual('boolean:true');
expect(hashKey(42)).toEqual('string:42');
expect(hashKey('42')).toEqual('string:42');

```