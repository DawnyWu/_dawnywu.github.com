---
layout: post
title:  "google supercharged"
date:   2016-11-21
categories: js
---

### sidebar transition

***显示sidebar***

```css
// 开始状态
.side-nav__container {
  // 注意这个
  transform: translateX(-102%);
  display: flex;
  flex-direction: column;
  will-change: transform;
}

// 变化后结果， 添加class visible, nav恢复到原来位置，显示出来
.side-nav--visible .side-nav__container {
  transform: none;
}

// 添加transition css
.side-nav--visible.side-nav--animatable .side-nav__container {
  transition: transform 1s ease-in-out;
}
```

```js
showSideNav () {
  // 先添加transition相关的class
  this.sideNavEl.classList.add('side-nav--animatable');
  this.sideNavEl.classList.add('side-nav--visible');
  this.detabinator.inert = false;
  // 下边这句优点问题，transitionend时间触发过早，设动动画时间3s,不到3s就会触发这个
  this.sideNavEl.addEventListener('transitionend', this.onTransitionEnd);
}

onTransitionEnd (evt) {
  console.log('transition end')
  this.sideNavEl.classList.remove('side-nav--animatable');
  this.sideNavEl.removeEventListener('transitionend', this.onTransitionEnd);
}
```

***隐藏sidebar的时候***

class是 js-side-nav-container side-nav__container

```js
hideSideNav () {
  this.sideNavEl.classList.add('side-nav--animatable');
  this.sideNavEl.classList.remove('side-nav--visible');
  this.detabinator.inert = true;
  this.sideNavEl.addEventListener('transitionend', this.onTransitionEnd);
}
```

```css
// 前后结果都已经写过了，现在只需注明transition动画即可
.side-nav--animatable .side-nav__container {
  transition: transform 3s cubic-bezier(0,0,0.3,1);
}
```

