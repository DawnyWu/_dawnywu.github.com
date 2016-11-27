---
layout: post
title:  "animation html"
date:   2016-11-21
categories: js
---

### perspective

让视角远离页面，不添加这个的话，子元素的`translateZ`什么的都会没有用的

### 使用translateZ()

如果要使用translateZ(), 外边的container css中必须要设置perspective: 500px;


transform: translateZ(-100px) rotateX(0deg) scale(0.9), offset: 0.15

### transform-style

* preserve-3d 不同3d元素之间的z轴的关系会保持，转的时候由于在立体空间的缘故，一些元素会偏离本来位置
* flat 没有相对z轴的关系，大家原地改变而已


### transform-origin

选择变形的原点

* The first value is the horizontal position
* the second value is the vertical
* the third value represents the position on the Z axis. The third value will only work if you are using 3D transforms, and it cannot be a percentage.

### overflow-scrolling

* touch: 像iphone手机一样的手指离开屏幕后，页面依然会滚动

* auto:  离开后不滚动

### position: -webkit-sticky;

例子 https://news.google.com/， 目前很多浏览器不支持,包括我的chrome

本来在自己的位置，往下滚动的时候才会固定到一个位置不懂

```js
.sticky {
  position: -webkit-sticky;
  position: -moz-sticky;
  position: -ms-sticky;
  position: -o-sticky;
  top: 15px;
}
```

old way

```js
<div class="header"></div>

<script>
var header = document.querySelector('.header');
var origOffsetY = header.offsetTop;

function onScroll(e) {
  window.scrollY >= origOffsetY ? header.classList.add('sticky') :
                                  header.classList.remove('sticky');
}

document.addEventListener('scroll', onScroll);
</script>
```

不过呢，给`scroll event`添加`handler`是很不好的

```js
var outerPane = $details.find(“.details-pane-outer”),
didScroll = false;

$(window).scroll(function() {
  didScroll = true;
});

setInterval(function() {
  if ( didScroll ) {
    didScroll = false;
    // Check your page position and then
    // Load in more results
  }
}, 250);
```

### scale factor

因为设置完translateZ之后,由于透视远离，元素大小会变，我们要scale(factor),factor的计算如下

The scale factor can be calculated with 1 + (translateZ * -1) / perspective. For example, if our viewport perspective is set to 1px and we translate an element -2px along the Z axis the correction scale factor would be 3:
