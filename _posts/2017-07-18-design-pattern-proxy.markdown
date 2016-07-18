---
layout: post
title:  "Design Patterns-"
date:   2016-07-18
categories: js
---

```js
var proxy = { ids: [],
delay: 50,
timeout: null,
callback: null,
context: null,
makeRequest: function (id, callback, context) {
// add to the queue this.ids.push(id);
this.callback = callback; this.context = context;
// set up timeout
if (!this.timeout) {
this.timeout = setTimeout(function () { proxy.flush();
}, this.delay);
￼￼Proxy | 165
} },
flush: function () {
http.makeRequest(this.ids, "proxy.handler");
// clear timeout and queue this.timeout = null; this.ids = [];
},
handler: function (data) {
var i, max;
// single video
if (parseInt(data.query.count, 10) === 1) {
proxy.callback.call(proxy.context, data.query.results.Video);
return; }
// multiple videos
for (i = 0, max = data.query.results.Video.length; i < max; i += 1) {
proxy.callback.call(proxy.context, data.query.results.Video[i]); }
} };
```