---
layout: post
title:  "google supercharged tips"
date:   2016-11-19
categories: js
---


###  LERP 

Lerp is the nickname for Linear Interpolation between two points

http://codepen.io/rachsmith/post/animation-tip-lerp

```js
// to run on each frame
function lerp(position, targetPosition) {
// update position by 20% of the distance between position and target position
  position.x += (targetPosition.x - position.x)*0.2;
  position.y += (targetPosition.y - position.y)*0.2;
}
```

原来是从1到9，可是直接从1到10的话，不太平滑

现在你设置一个值，比如说2，那么就变成了1，3，5，7，9，平滑了很多，不过会感觉变慢，会晚一些到达目标位置

```js
var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');
var width = window.innerWidth;
var height = window.innerHeight;
var x = window.innerWidth/2;
var y = window.innerHeight/2;
var ballX = x;
var ballY = y;
resize();

function drawBall() {
  ctx.beginPath();
  // instead of updating the ball position to the mouse position we will lerp 10% of the distance between the balls current position and the mouse position.
  ballX += (x - ballX)*0.1;
  ballY += (y - ballY)*0.1;
  ctx.arc(ballX, ballY, 40, 0, 2*Math.PI);
  ctx.fillStyle = '#9e356a';
  ctx.fill();
}

function loop() {
  ctx.clearRect(0, 0, width, height);
  drawBall();
  requestAnimationFrame(loop);
}

loop();

function touch(e) {
  x = e.originalEvent.touches[0].pageX;
  y = e.originalEvent.touches[0].pageY;
}

function mousemove(e) {
  x = e.pageX;
  y = e.pageY;
}

function resize() {
  width = canvas.width = window.innerWidth;
  height = canvas.height = window.innerHeight;
}

window.addEventListener('resize', resize);
window.addEventListener('touchstart', touch);
window.addEventListener('touchmove', touch);
window.addEventListener('mousemove', mousemove);
```

### contain: strict

