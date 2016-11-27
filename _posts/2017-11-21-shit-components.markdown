---
layout: post
title:  "狗日的组件研究"
date:   2016-11-21
categories: js
---

***getStylePrefix***

为了为style获取正确的prefix,因为不同系统下prefix不一样,以后可以考虑用autoprefix

```js
export const getStylePrefix = (style) => {
    const prefixs = [ 'webkit', 'Moz', 'ms', 'O' ];
    // 获取一个div所有的默认css，都是空
    const dom = document.createElement('div').style;
    // 样式在其中，返回''
    if (style in dom) return '';
    const newStyle = style.replace('-', ' ').replace(/(^|\s+)\w/g, s => s.toUpperCase()).replace(' ', '');
    for (const prefix of prefixs) {
        if ((prefix + newStyle) in dom) {
            return prefix;
        }
    }
    return null;
};
```

### scrollTags的this.props.children是干什么的？?

这个他妈的没搞错吧。。。。没搞错，就要这么一层层叠加计算

http://stackoverflow.com/questions/5598743/finding-elements-position-relative-to-the-document

```js
// 获取距离Body顶部的高度
export const getOffsetTop = (element) => {
  if (!element) return 0;
  let offsetTop = 0;
  let el = element;
  while (el.offsetParent) {
    offsetTop += el.offsetTop;
    el = el.offsetParent;
  }
  return offsetTop;
};
```

### 牛逼的animate函数

```js
// 使用
animate(node.scrollLeft, offset, 300, (scrollLeft) => {
  node.scrollLeft = scrollLeft;
});


// 使用的时候node是 `.zhilin-scroll-wrap`

export const animate = (begin, end, duration, onProgress, onEnd, curver = 'easeInOut') => {
  // s是函数运行最开始start time, 形似1479885176139
  const s = (new Date()).getTime();

  // const isObject = typeof begin === 'object' && !isArray(begin);
  const isNumber = typeof begin === 'number';

  const b = begin;
  let c;
  let keys;
  if (isNumber) {
    c = end - b; // change
  } else {
    keys = Object.keys(end);
    c = {};
    for (const key of keys) {
      c[key] = end[key] - b[key];
    }
  }
  const d = duration || 500;

  // t是过了多少时间
  // b是begin的数值
  // c是end 减去 begin
  // d是duration
  // 返回计算后动画进行中的值
  const calc = (t) => {
    if (typeof curver === 'function') return curver(t, b, c, d);
    // begin isNumber
    if (isNumber) return tween(curver, t, b, c, d);
    const r = {};
    for (const key of keys) {
      r[key] = tween(curver, t, b[key], c[key], d);
    }
    return r;
  };

  (function execute() {
      // 现在的时间-开始的时间, t 是过了多少时间
      let t = (new Date()).getTime() - s;
      if (t > d) {
          t = d;
          // 调用onProgress callback
          onProgress && onProgress(calc(t));
          // 调用onEnd callback
          onEnd && onEnd();
          return;
      }
      //  (scrollLeft) => { node.scrollLeft = scrollLeft }
      onProgress && onProgress(calc(t));
      // getRAF当然就是为了找到对应浏览器版本的requestAnimationFrame
      getRAF(execute);
  })();
};
```

### github google chrome 的 getRAF等

```js
function getRAF(w) {
  w = w || window;
  if (w.requestAnimationFrame)
    return w.requestAnimationFrame;
  if (w.webkitRequestAnimationFrame)
    return w.webkitRequestAnimationFrame;
  if (w.mozRequestAnimationFrame)
    return w.mozRequestAnimationFrame;
  if (w.oRequestAnimationFrame)
    return w.oRequestAnimationFrame;
  if (w.msRequestAnimationFrame)
    return w.msRequestAnimationFrame;
  return undefined;
}

function getVisibilityState(d) {
  d = d || window;
  if (d.visibilityState)
    return d.visibilityState;
  if (d.webkitVisibilityState)
    return d.webkitVisibilityState;
  if (d.mozVisibilityState)
    return d.mozVisibilityState;
  if (d.oVisibilityState)
    return d.oVisibilityState;
  if (d.msVisibilityState)
    return d.msVisibilityState;
  return undefined;
}        
```

对比

```js
// 获取requestAnimationFrame
export const getRAF = (func) => {
  const w = window;
  const prefixs = [ 'r', 'webkitR', 'mozR', 'msR', 'oR' ];
  // for(let i = 0; i < prefixs.length; i++){
  for (let prefix of prefixs) {
    prefix = `${prefix}equestAnimationFrame`;
    if (prefix in w) return w[prefix](func);
  }
  return setTimeout(func, 1e3 / 60);
};
```



