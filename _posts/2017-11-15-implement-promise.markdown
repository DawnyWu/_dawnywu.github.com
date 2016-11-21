---
layout: post
title:  "implement promise"
date:   2016-11-15
categories: js
---

DemoPromise is a class with three prototype methods:

DemoPromise.prototype.resolve(value)
DemoPromise.prototype.reject(reason)
DemoPromise.prototype.then(onFulfilled, onRejected)

目标：

```js
const dp = new DemoPromise();
dp.resolve('abc');
dp.then(function (value) {
    console.log(value); // abc
});
```

### then

```js

then(onFullfilled, onRejected) {
  const self = this
  const fulfilledTask = function () {
    onFulfilled(self.promiseResult);
  };
  const rejectedTask = function () {
    onRejected(self.promiseResult);
  };
  switch (this.promiseState) {
    case 'pending':
      this.fulfillReactions.push(fulfilledTask);
      this.rejectReactions.push(rejectedTask);
      break;
    case 'fulfilled':
      addToTaskQueue(fulfilledTask);
      break;
    case 'rejected':
      addToTaskQueue(rejectedTask);
      break;
  }
}

function addToTaskQueue(task) {
  setTimeout(task, 0);
}

```

### resolve

```js
resolve(value) {
    // 如果promise已经是settled，那么就返回
    if (this.promiseState !== 'pending') return;
    this.promiseState = 'fulfilled';
    this.promiseResult = value;
    this._clearAndEnqueueReactions(this.fulfillReactions);
    return this; // enable chaining
}
_clearAndEnqueueReactions(reactions) {
    this.fulfillReactions = undefined;
    this.rejectReactions = undefined;
    reactions.map(addToTaskQueue);
}
```

### chaining

