---
layout: post
title:  "animation"
date:   2016-12-12
categories: java
---

***reflow repaint***

`reflow`: browser has to recalculate the element’s geometry 

`repaint`: render the image of the page’s new state 

every modern browser is smart enough to repaint only the changed area of the page

***compositing optimization***

对动画的css有要求

* does not affect the document’s flow,
* does not depend on the document’s flow,
* does not cause a repaint.


`transform` and `opacity` are the only CSS properties that meet the conditions above.

***implicit composing***

A browser will promote an element to a compositing layer for many reasons, just a few of which are:

* 3D transforms: translate3d, translateZ and so on;
* <video>, <canvas> and <iframe> elements;
* animation of transform and opacity via Element.animate();
* animation of transform and opacity via СSS transitions and animations;
* position: fixed;
* will-change;
* filter;

### resistance function

```js
resistanceFunction = (t) => Math.min(1, t / 2.5)

let distResisted = this.resistanceFunction(this.dist / this.distTreshold) * Math.min(this.distMax, this.dist);
```

### box shadow

pullToRefresh中的样式

```css
box-shadow: inset 0 -3px 5px rgba(0, 0, 0, 0.12);
```


### refresh action

```js
_timeout = setTimeout(() => {
  // What will the pull to refresh trigger? You can return a promise. Defaults to window.location.reload()
  const retval = onRefresh(onReset);

  if (retval && typeof retval.then === 'function') {
    retval.then(() => onReset());
  }

  if (!retval && !onReset.length) {
    onReset();
  }
}, refreshTimeout);

function onReset() {
  const { cssProp, ptrElement } = _SETTINGS;

  ptrElement.classList.remove(`${classPrefix}refresh`);
  ptrElement.style[cssProp] = '0px';
}
```