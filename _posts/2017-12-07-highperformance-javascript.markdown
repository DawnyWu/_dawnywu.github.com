---
layout: post
title:  "highperformance java"
date:   2016-12-07
categories: java
---
### Loading scripts

***defer***

The `defer` attribute indicates that the script contained within the element is not going to modify the DOM and therefore execution can be safely deferred until a later point in time.

javascirpt文件还是会照常下载，不过执行会defer到`onload event`

Any <script> element marked with defer will not execute until after the DOM has been completely loaded; 

this holds true for `inline scripts` as well as for `external script` files

***Dynamic Script Elements***

```js
var script = document.createElement("script");
script.type = "text/javascript";
script.src = "file1.js"; 
document.getElementsByTagName("head")[0].appendChild(script);
```
 
有`onload`事件可以监听

```js
var script = document.createElement("script") 
script.type = "text/javascript";

script.onload = function(){
  alert("Script loaded!"); 
};

script.src = "file1.js";
document.getElementsByTagName("head")[0].appendChild(script);
```

The file begins downloading as soon as the element is added to the page. 

### Scope

The internal `[[Scope]]` property contains a collection of objects representing the scope in which the function was created.

This collection is called the `function’s scope chain` and it determines the data that a function can access.

```js
function add(num1, num2){ 
  var sum = num1 + num2;
  return sum; 
}
```

The `add` function’s scope chain is later used when the function is executed. 


```js
function assignEvents(){
  var id = "xdi9592";
  document.getElementById("save-btn").onclick = function(event){
    saveDocument(id); 
  };
}
````

















### Dom

```js
function innerHTMLLoop() {
  for (var count = 0; count < 15000; count++) {
    document.getElementById('here').innerHTML += 'a'; 
  }
}
```

不放到循环里，只运行一次

```js
function innerHTMLLoop2() {
  var content = '';
  for (var count = 0; count < 15000; count++) {
    content += 'a'; 
  }
  document.getElementById('here').innerHTML += content; 
}
```