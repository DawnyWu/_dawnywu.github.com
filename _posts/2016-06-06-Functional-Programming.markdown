---
layout: post
title:  "Functional Programming"
date:   2016-06-05 17:15:06 +0800
categories: jekyll update
---
## 函数式编程中的错误处理

### 什么是lifting?

`var zoltar = compose(map(console.log), map(fortune), getAge(moment()));`

At the time of calling, a function can be surrounded by map, which transforms it from a non-functory function to a functory one, in informal terms. We call this process lifting.


### Side Effect 
Any interaction with the world outside of a function is a side effect

包括但不限于以下：

* changing the file system
* inserting a record into a database
* making an http call
* mutations
* printing to the screen / logging
* obtaining user input
* querying the DOM
* accessing system state

### Impure function to pure function

{% highlight javascript %}
var pureHttpCall = memoize(function(url, params) {
  return function() {
    return $.getJSON(url, params);
  };
});
{% endhighlight %}

```javascript
var getFromStorage = function(key) {
  return function() {
    return localStorage[key];
  };
};
```


[jekyll-docs]: http://jekyllrb.com/docs/home
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-talk]: https://talk.jekyllrb.com/
